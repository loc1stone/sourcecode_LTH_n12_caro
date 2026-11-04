{-# LANGUAGE ScopedTypeVariables #-}
-- | RANH GIỚI HIỆU ỨNG: nơi duy nhất có IO, luồng và đồng bộ hóa.
--
-- Các tác vụ chạy đồng thời:
--
--   * 'inputThread'    đọc dòng người gõ        -> đẩy 'EvLine'
--   * 'clockThread'    mỗi giây một nhịp        -> đẩy 'EvTick'
--   * 'autosaveThread' tự lưu khi ván thay đổi  (chờ bằng STM @retry@)
--   * tác vụ máy       tính nước đi song song   -> đẩy 'EvBotMove'
--   * 'netReaderThread' đọc gói tin từ đối thủ  -> đẩy 'EvNet'
--   * luồng chính      'runLoop': lấy sự kiện, chạy hàm thuần 'step', thực thi hiệu ứng
--
-- Cơ chế đồng bộ hóa:
--
--   * 'TQueue' (STM): hàng đợi sự kiện — mọi luồng chỉ GHI vào đây, nên không có
--     hai luồng cùng sửa trạng thái ván cờ.
--   * 'TVar' 'Session' (STM): trạng thái duy nhất, chỉ luồng chính ghi; luồng tự
--     lưu chỉ đọc một ảnh chụp nhất quán (atomically).
--   * 'MVar' khóa tệp: hai nơi cùng ghi tệp lưu thì lần lượt.
--   * Số phiên bản 'sVersion': loại bỏ kết quả "cũ" của máy (xem 'onBotMove').
--   * Chỉ luồng chính được in ra màn hình: các dòng không bị xen lẫn.
module Caro.App
  ( Env (..), Output (..), Severity (..), textOutput
  , newEnv, newEnvWith, post
  , runLoop, runEffects
  , inputThread, clockThread, autosaveThread, netReaderThread
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.IO
import System.IO.Error (isEOFError)

import Caro.Parallel (parBestMove)
import Caro.Pretty
import Caro.Protocol
import Caro.Session
import Caro.Storage
import Caro.Types

data Severity = SevInfo | SevWarn | SevQuiet
  deriving (Eq, Show)

-- | Nơi hiển thị kết quả: dòng lệnh ('textOutput') hay giao diện đồ họa ("Caro.Gui").
-- Phần còn lại của chương trình không cần biết đang dùng loại nào.
data Output = Output
  { outLine   :: Severity -> String -> IO ()   -- ^ một thông báo
  , outBoard  :: GameState -> IO ()            -- ^ vẽ lại bàn cờ
  , outPrompt :: IO ()                         -- ^ nhắc người dùng nhập lệnh
  }

-- | Hiển thị dạng văn bản qua một hàm ghi chuỗi.
textOutput :: (String -> IO ()) -> Output
textOutput out = Output
  { outLine   = \_ t -> out (t ++ "\n")
  , outBoard  = out . pretty
  , outPrompt = out "> "
  }

data Env = Env
  { envQueue   :: TQueue Event
  , envSession :: TVar Session
  , envOut     :: Output            -- ^ chỉ luồng chính gọi (người ghi duy nhất)
  , envLock    :: MVar ()           -- ^ khóa ghi tệp lưu
  , envPeer    :: Maybe Handle      -- ^ kết nối tới đối thủ (chế độ mạng)
  , envWorkers :: Int               -- ^ số luồng tính nước đi của máy
  }

newEnv :: (String -> IO ()) -> Maybe Handle -> Int -> Session -> IO Env
newEnv = newEnvWith . textOutput

newEnvWith :: Output -> Maybe Handle -> Int -> Session -> IO Env
newEnvWith out peer workers s =
  Env <$> newTQueueIO <*> newTVarIO s <*> pure out <*> newMVar () <*> pure peer <*> pure workers

post :: Env -> Event -> IO ()
post env = atomically . writeTQueue (envQueue env)

-- | Vòng lặp chính: lấy sự kiện -> hàm thuần 'step' -> thực thi hiệu ứng.
runLoop :: Env -> IO ()
runLoop env = do
  ev <- atomically (readTQueue (envQueue env))
  -- Cập nhật trạng thái trong MỘT giao dịch STM: luồng tự lưu không bao giờ
  -- thấy trạng thái nửa vời.
  fx <- atomically $ do
    s <- readTVar (envSession env)
    let (s', effs) = step ev s
    writeTVar (envSession env) s'
    pure effs
  runEffects env fx
  when (wantsPrompt ev fx) (outPrompt (envOut env))
  done <- sDone <$> readTVarIO (envSession env)
  unless done (runLoop env)

wantsPrompt :: Event -> [Effect] -> Bool
wantsPrompt ev fx = not (any isStop fx) && case ev of
  EvTick   -> False
  EvLine _ -> True
  _        -> not (null fx)
  where isStop Stop = True
        isStop _    = False

runEffects :: Env -> [Effect] -> IO ()
runEffects env = mapM_ run
  where
    line = outLine (envOut env)
    run eff = case eff of
      Say t       -> line SevInfo t
      Warn t      -> line SevWarn t
      Quiet t     -> line SevQuiet t
      ShowBoard g -> outBoard (envOut env) g
      Send m      -> forM_ (envPeer env) $ \h ->
        (hPutStrLn h (renderMsg m) >> hFlush h)
          `catch` \(e :: IOException) -> line SevWarn ("Không gửi được tới đối thủ: " ++ show e)
      StartBot v depth g -> void (forkIO (botTask env v depth g))
      SaveTo path g -> do
        r <- saveGame (envLock env) path g
        case r of
          Left e   -> line SevWarn ("Lỗi khi lưu: " ++ e)
          Right () -> line SevInfo ("Đã lưu ván cờ vào " ++ path ++ ".")
      LoadFrom path -> loadGame path >>= post env . EvLoaded path
      Stop        -> pure ()

-- | Tác vụ nền của máy: tính song song rồi báo kết quả về hàng đợi.
botTask :: Env -> Int -> Int -> GameState -> IO ()
botTask env v depth g = do
  r <- try (parBestMove (envWorkers env) depth g)
  post env $ case r of
    Right (Just p) -> EvBotMove v p
    Right Nothing  -> EvNote "Máy không tìm được nước đi."
    Left (e :: SomeException) -> EvNote ("Máy gặp lỗi: " ++ show e)

-- ---------------------------------------------------------------------------
-- Các luồng nền

inputThread :: Env -> IO ()
inputThread env = loop
  where
    loop = do
      r <- try getLine
      case r of
        Right l -> post env (EvLine l) >> loop
        Left (e :: IOException)
          | isEOFError e -> post env EvEof
          | otherwise    -> post env EvEof

clockThread :: Env -> IO ()
clockThread env = forever (threadDelay 1000000 >> post env EvTick)

-- | Tự lưu: ngủ cho đến khi 'sVersion' khác bản đã lưu — chờ bằng 'check' (STM
-- tự 'retry' khi điều kiện sai, không cần vòng lặp thăm dò), đợi thêm một nhịp để
-- gom các thay đổi liên tiếp, rồi lưu ảnh chụp mới nhất.
-- (Phiên bản khởi đầu cố định là 0 thay vì "đọc lúc luồng bắt đầu chạy": cách sau
-- sẽ bỏ sót thay đổi nếu luồng được lập lịch muộn — một tranh chấp do kiểm thử phát hiện.)
autosaveThread :: Env -> IO ()
autosaveThread env = go 0   -- phiên bản 0 = ván vừa tạo/nạp, chưa có gì mới để lưu
  where
    snapshot = do
      s <- readTVar (envSession env)
      pure (sVersion s, sGame s, sSavePath s)
    go seen = do
      _ <- atomically $ do
        (v, _, _) <- snapshot
        check (v /= seen)
      threadDelay 800000
      (v, g, path) <- atomically snapshot
      r <- saveGame (envLock env) path g
      post env $ case r of
        Right () -> EvQuiet ("(đã tự động lưu: " ++ show (length (gsHistory g)) ++ " nước)")
        Left e   -> EvNote ("Tự động lưu thất bại: " ++ e)
      go v

netReaderThread :: Env -> Handle -> IO ()
netReaderThread env h = loop
  where
    loop = do
      r <- try (hGetLine h)
      case r of
        Left (_ :: IOException) -> post env EvNetClosed
        Right l -> do
          post env $ case parseMsg l of
            Left e  -> EvNetBad (show e)
            Right m -> EvNet m
          loop
