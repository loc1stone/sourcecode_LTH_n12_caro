module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (IOException, try)
import Control.Monad (forM_, void, when)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO

import Caro.App
import Caro.Ascii (unaccent)
import Caro.Cli
import Caro.Gui
import Caro.Net
import Caro.Parallel (parBestMove)
import Caro.Parser (parseErrorSummary)
import Caro.Pretty
import Caro.Protocol
import Caro.Rules
import Caro.Session
import Caro.Storage
import Caro.Types

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetEncoding stdin utf8
  args <- getArgs
  case parseArgs args of
    Left err -> do
      let conv = if "--ascii" `elem` args then unaccent else id
      hPutStrLn stderr (conv err)
      putStr (conv usage)
      exitWith (ExitFailure 2)
    Right o
      | oHelp o   -> putStr (if oAscii o then unaccent usage else usage)
      | otherwise -> run o

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)

run :: Options -> IO ()
run o = case oMode o of
  BenchMode
    | oGui o     -> bye "--gui không dùng chung với --bench."
    | otherwise  -> runBench conv
  Local          -> offline PvP
  VsMachine lvl  -> offline (VsBot (oHuman o) lvl)
  HostMode port  -> do
    cfg <- either (bye . pretty) pure (mkConfig (oSize o) (oVariant o) Unlimited)
    note
    say ("Đang chờ đối thủ kết nối ở cổng " ++ show port ++ " ...")
    h <- tryIO (hostGame port)
    hPutStrLn h (renderMsg (Hello (oSize o) (oVariant o)))
    hFlush h
    play (Online X) (Just h) (newGame cfg)
  JoinMode host port -> do
    note
    h <- tryIO (joinGame host port)
    line <- tryIO (hGetLine h)
    case parseMsg line of
      Right (Hello n v) -> do
        cfg <- either (bye . pretty) pure (mkConfig n v Unlimited)
        play (Online O) (Just h) (newGame cfg)
      Right _  -> bye "Chủ phòng gửi gói tin mở đầu không đúng."
      Left e   -> bye ("Gói tin mở đầu không đọc được: " ++ parseErrorSummary e)
  where
    conv = if oAscii o then unaccent else id
    say  = putStrLn . conv
    bye  = die' . conv
    note = case oTime o of
      Unlimited -> pure ()
      _         -> say "Lưu ý: chế độ mạng không dùng giới hạn thời gian; bỏ qua --time."
    tryIO act = do
      r <- try act
      case r of
        Right x -> pure x
        Left e  -> bye ("Lỗi mạng: " ++ show (e :: IOException))
    offline mode = do
      cfg  <- either (bye . pretty) pure (mkConfig (oSize o) (oVariant o) (oTime o))
      game <- if oResume o then resume cfg else pure (newGame cfg)
      play mode Nothing game
    resume cfg = do
      r <- loadGame (oSave o)
      case r of
        Right g  -> say ("Đã nạp ván cờ từ " ++ oSave o ++ ".") >> pure g
        Left err -> do
          say ("Không nạp được " ++ oSave o ++ " (" ++ err ++ "); bắt đầu ván mới.")
          pure (newGame cfg)
    play mode peer game
      | oGui o    = playGui mode peer game
      | otherwise = playTerminal mode peer game

    -- Giao diện đồ họa: cùng lõi 'step', cùng các luồng nền; khác ở chỗ đọc
    -- thao tác từ máy chủ HTTP thay vì từ bàn phím, và ghi thông báo vào nhật ký.
    playGui mode peer game = do
      caps <- getNumCapabilities
      lg   <- newTVarIO emptyLog
      let (s0, fx0) = startSession (newSession mode game (oSave o))
      env <- newEnvWith (guiOutput lg say) peer (max 2 caps) s0
      r <- try (bindGui (oPort o))
      (sock, port) <- case r of
        Right x -> pure x
        Left e  -> bye ("Không mở được cổng " ++ show (oPort o) ++ ": " ++ show (e :: IOException)
                         ++ "\nThử cổng khác: --port 9000")
      let url = "http://localhost:" ++ show port ++ "/"
      runEffects env fx0
      say ("Giao diện đồ họa: " ++ url ++ "   (Ctrl-C để thoát)")
      forM_ [clockThread env, autosaveThread env, serveGui (Gui env lg port) sock] (void . forkIO)
      forM_ peer $ \h -> forkIO (netReaderThread env h)
      when (oBrowser o) (openBrowser url)
      runLoop env

    playTerminal mode peer game = do
      caps <- getNumCapabilities
      let (s0, fx0) = startSession (newSession mode game (oSave o))
      env <- newEnv out peer (max 2 caps) s0
      runEffects env fx0
      out "Gõ 'help' để xem lệnh.\n"
      outPrompt (envOut env)
      forM_ [inputThread env, clockThread env, autosaveThread env] (void . forkIO)
      forM_ peer $ \h -> forkIO (netReaderThread env h)
      runLoop env
    out s = putStr (conv s) >> hFlush stdout

-- | So sánh thời gian tính một nước đi (độ sâu 5) với 1, 2, 4 luồng.
runBench :: (String -> String) -> IO ()
runBench conv = do
  caps <- getNumCapabilities
  putStrLn . conv $ ("Số capability (-N): " ++ show caps)
  case replay defaultConfig benchMoves of
    Left e  -> die' (conv (pretty e))
    Right g -> do
      putStr (conv (pretty g))
      results <- mapM (timeIt g) [1, 2, 4]
      let answers = map snd results
      putStrLn . conv $ (if all (== head answers) answers
                  then "Mọi cấu hình cho CÙNG một nước đi (kết quả không phụ thuộc số luồng)."
                  else "CẢNH BÁO: các cấu hình cho nước đi khác nhau!")
  where
    timeIt g w = do
      t0 <- getMonotonicTime
      r  <- parBestMove w 5 g
      t1 <- getMonotonicTime
      putStrLn . conv $ ("  " ++ show w ++ " luồng: " ++ show (round ((t1 - t0) * 1000) :: Int)
                ++ " ms -> " ++ maybe "?" pretty r)
      pure (w :: Int, r)

benchMoves :: [Pos]
benchMoves =
  [ Pos 7 7, Pos 7 8, Pos 8 8, Pos 6 6, Pos 8 7, Pos 8 9, Pos 6 8, Pos 9 9
  , Pos 9 7, Pos 5 9, Pos 7 6, Pos 7 9 ]
