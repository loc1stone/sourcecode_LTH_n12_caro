{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Bộ kiểm thử không phụ thuộc thư viện ngoài (chỉ dùng base/stm/containers):
-- một "khung" nhỏ tự viết + kiểm thử theo tính chất (property) với bộ sinh số giả ngẫu nhiên.
--
-- Chạy:  cabal test      (hoặc: cabal run caro-test)
module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bits (shiftR)
import Data.Char (isAscii, toUpper)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit
import System.IO (hSetEncoding, stdout, utf8)

import Caro.AI
import Caro.Ascii
import Caro.App
import Caro.Cli
import Caro.Gui
import Caro.Json
import Caro.Parallel
import Caro.Parser
import Caro.Pretty
import Caro.Protocol
import Caro.Rules
import Caro.Session
import Caro.Storage
import Caro.Types

-- ---------------------------------------------------------------------------
-- Khung kiểm thử tối giản

type Test = (String, IO Bool)

t :: String -> Bool -> Test
t name b = (name, pure b)

tIO :: String -> IO Bool -> Test
tIO = (,)

runTest :: Test -> IO Bool
runTest (name, act) = do
  r <- try act
  case r of
    Right True  -> putStrLn ("  [ok]   " ++ name) >> pure True
    Right False -> putStrLn ("  [FAIL] " ++ name) >> pure False
    Left e      -> putStrLn ("  [FAIL] " ++ name ++ " (ngoại lệ: " ++ show (e :: SomeException) ++ ")") >> pure False

section :: String -> [Test] -> IO [Bool]
section title ts = putStrLn title >> mapM runTest ts

-- ---------------------------------------------------------------------------
-- Tiện ích

-- | Đọc tọa độ dạng "h8" bằng chính parser của chương trình.
at :: String -> Pos
at s = case parseCommand s of
  Right (CMove p) -> p
  other'          -> error ("tọa độ sai trong kiểm thử: " ++ s ++ " -> " ++ show other')

moves :: Config -> [String] -> Either MoveError GameState
moves cfg = replay cfg . map at

cfgV, cfgF :: Config
cfgV = defaultConfig                              -- caro 15x15
cfgF = defaultConfig { cfgVariant = Freestyle }

isWon :: Status -> Bool
isWon (Won _ _) = True
isWon _         = False

status :: Either MoveError GameState -> Maybe Status
status = either (const Nothing) (Just . gsStatus)

-- Quân của đối thủ đặt xa nhau để không tạo/chắn dãy nào đáng kể.
fillers :: [String]
fillers = ["a1", "c1", "e1", "g1", "a3", "c3", "e3", "g3", "a5", "c5"]

-- | Xen kẽ nước của X (đã cho) với quân "đệm" của O.
interleave :: [String] -> [String]
interleave xs = concat (zipWith (\x o -> [x, o]) xs fillers)

-- | Như 'interleave' nhưng bỏ nước đệm cuối (để X đi nước cuối cùng).
xWinning :: [String] -> [String]
xWinning xs = init (interleave xs)

-- Bộ sinh số giả ngẫu nhiên (LCG) để kiểm thử theo tính chất, kết quả lặp lại được.
nextRand :: Word64 -> Word64
nextRand s = s * 6364136223846793005 + 1442695040888963407

pick :: Word64 -> Int -> Int
pick s n = fromIntegral (s `shiftR` 33) `mod` n

-- | Chơi ngẫu nhiên tối đa @k@ nước; trả về mọi trạng thái trung gian.
randomPlay :: Word64 -> Int -> GameState -> [GameState]
randomPlay seed k g0 = go seed k g0
  where
    go _ 0 g = [g]
    go s n g = case legalMoves g of
      [] -> [g]
      ms -> let s' = nextRand s
                p  = ms !! pick s' (length ms)
            in case applyMove p g of
                 Right g' -> g : go s' (n - 1) g'
                 Left _   -> [g]

-- | Kiểm tra thắng BẰNG CÁCH QUÉT TOÀN BÀN (cài đặt độc lập với 'winningLine',
-- dùng để đối chiếu).
bruteWon :: Config -> Board -> Bool
bruteWon cfg b = or
  [ k >= winLength && (cfgVariant cfg == Freestyle || openA || openB)
  | (p, pl) <- M.toList b
  , d <- directions
  , M.lookup (addPos (negPos d) p) b /= Just pl          -- p là đầu dãy
  , let run   = takeWhile (\q -> M.lookup q b == Just pl) (iterate (addPos d) p)
        k     = length run
        endA  = addPos (negPos d) p
        endB  = addPos d (last run)
        open q = inBounds (cfgSize cfg) q && M.notMember q b
        openA = open endA
        openB = open endB
  ]

-- ---------------------------------------------------------------------------
-- Các nhóm kiểm thử

ruleTests :: [Test]
ruleTests =
  [ t "bàn trống: X đi trước" (gsTurn (newGame cfgV) == X)
  , t "đặt quân đổi lượt" (fmap gsTurn (moves cfgV ["h8"]) == Right O)
  , t "ô đã có quân bị từ chối"
      (status (moves cfgV ["h8", "h8"]) == Nothing
        && moves cfgV ["h8", "h8"] == Left (Occupied (at "h8")))
  , t "ngoài bàn bị từ chối (cột)" (moves cfgV ["p1"] == Left (OutOfBounds (at "p1")))
  , t "ngoài bàn bị từ chối (hàng 0)" (moves cfgV ["a0"] == Left (OutOfBounds (Pos (-1) 0)))
  , t "thắng hàng ngang"
      (status (moves cfgF (xWinning ["h8", "i8", "j8", "k8", "l8"]))
         == Just (Won X (FiveInRow (map at ["h8", "i8", "j8", "k8", "l8"]))))
  , t "thắng hàng dọc" (fmap isWon (status (moves cfgV (xWinning ["h8", "h9", "h10", "h11", "h12"]))) == Just True)
  , t "thắng chéo xuôi" (fmap isWon (status (moves cfgV (xWinning ["f6", "g7", "h8", "i9", "j10"]))) == Just True)
  , t "thắng chéo ngược" (fmap isWon (status (moves cfgV (xWinning ["j6", "i7", "h8", "g9", "f10"]))) == Just True)
  , t "4 quân chưa thắng" (status (moves cfgV (xWinning ["h8", "i8", "j8", "k8"])) == Just InProgress)
  , t "O cũng thắng được"
      (fmap isWon (status (moves cfgV ["a1", "h8", "c1", "h9", "e1", "h10", "g1", "h11", "a3", "h12"])) == Just True)
  , t "5 quân bị chặn cả hai đầu: caro KHÔNG thắng, freestyle thắng"
      (let seqn = ["h8", "g8", "i8", "m8", "j8", "a1", "k8", "c1", "l8"]
       in status (moves cfgV seqn) == Just InProgress
            && fmap isWon (status (moves cfgF seqn)) == Just True)
  , t "5 quân một đầu mở: caro thắng"
      (fmap isWon (status (moves cfgV ["h8", "g8", "i8", "a1", "j8", "c1", "k8", "e1", "l8"])) == Just True)
  , t "biên: dãy sát mép trái, đầu kia mở -> thắng (caro)"
      (fmap isWon (status (moves cfgV ["a8", "a1", "b8", "c1", "c8", "e1", "d8", "g1", "e8"])) == Just True)
  , t "biên: dãy sát mép + đối thủ chặn đầu kia -> không thắng (caro)"
      (status (moves cfgV ["a8", "f8", "b8", "a1", "c8", "c1", "d8", "e1", "e8"]) == Just InProgress)
  , t "6 quân liên tiếp (overline) thắng ở freestyle"
      (fmap (\s -> case s of Won _ (FiveInRow l) -> length l; _ -> 0)
            (status (moves cfgF ["h8", "a1", "i8", "c1", "j8", "e1", "l8", "g1", "m8", "a3", "k8"])) == Just 6)
  , t "sau khi thắng không đi tiếp được"
      (let Right g = moves cfgV (xWinning ["h8", "i8", "j8", "k8", "l8"])
       in applyMove (at "o15") g == Left GameOver)
  , t "bàn đầy không ai thắng -> Hòa" drawTest
  , t "undo bàn trống -> NothingToUndo" (undoMove (newGame cfgV) == Left NothingToUndo)
  , t "undo hoàn lại đúng trạng thái trước"
      (let Right g1 = moves cfgV ["h8"]
           Right g2 = applyMove (at "i9") g1
       in undoMove g2 == Right g1)
  , t "undo sau khi thắng đưa ván về đang chơi"
      (let Right g = moves cfgV (xWinning ["h8", "i8", "j8", "k8", "l8"])
       in fmap gsStatus (undoMove g) == Right InProgress)
  , t "đầu hàng: đối thủ thắng"
      (gsStatus (resign X (newGame cfgV)) == Won O Resignation)
  , t "đầu hàng khi ván đã xong: không đổi"
      (let g = resign X (newGame cfgV) in resign O g == g)
  , t "hết giờ: người đến lượt thua"
      (let cfg = cfgV { cfgTime = PerMove 5 }
           g   = iterate tick (newGame cfg) !! 5
       in gsStatus g == Won O Timeout)
  , t "chưa hết giờ thì còn InProgress"
      (let cfg = cfgV { cfgTime = PerMove 5 }
       in gsStatus (iterate tick (newGame cfg) !! 4) == InProgress)
  , t "đi nước mới thì đồng hồ được đặt lại"
      (let cfg = cfgV { cfgTime = PerMove 30 }
           g   = iterate tick (newGame cfg) !! 10
       in fmap gsClock (applyMove (at "h8") g) == Right 30)
  , t "không giới hạn thời gian thì tick không làm gì"
      (let g = newGame cfgV in tick g == g)
  , t "mkConfig từ chối cỡ bàn và thời gian sai"
      (mkConfig 3 VietCaro Unlimited == Left (BadSize 3)
        && mkConfig 27 VietCaro Unlimited == Left (BadSize 27)
        && mkConfig 15 VietCaro (PerMove 1) == Left (BadTime 1)
        && mkConfig 15 Freestyle (PerMove 30) /= Left (BadTime 30))
  , t "walk: danh sách vô hạn nhưng dừng nhờ đánh giá lười"
      (walk M.empty X (Pos 0 0) (Pos 0 1) == []
        && length (walk (M.fromList [(Pos 0 c, X) | c <- [1 .. 4]]) X (Pos 0 0) (Pos 0 1)) == 4)
  ]
  where
    -- Mẫu tô màu không có 5 quân liên tiếp theo hướng nào (xem chứng minh trong README).
    colorAt r c = if ((r `div` 2) + c) `mod` 2 == 0 then X else O
    drawTest =
      let n    = 9
          cfg  = cfgV { cfgSize = either (error . show) id (mkSize n) }
          cells = [Pos r c | r <- [0 .. n - 1], c <- [0 .. n - 1]]
          lastP = Pos (n - 1) (n - 1)
          board = M.fromList [(p, colorAt (posRow p) (posCol p)) | p <- cells, p /= lastP]
          g     = (newGame cfg) { gsBoard = board, gsTurn = colorAt (n - 1) (n - 1) }
      in fmap gsStatus (applyMove lastP g) == Right Draw

-- | Đối chiếu phát hiện thắng tăng dần với quét toàn bàn, trên nhiều ván ngẫu nhiên.
propertyTests :: [Test]
propertyTests =
  [ t "thắng tăng dần == quét toàn bàn (caro & freestyle, 60 ván ngẫu nhiên)" $
      and [ all ok (randomPlay seed 120 (newGame cfg))
          | seed <- [1 .. 30], v <- [Freestyle, VietCaro]
          , let cfg = cfgV { cfgSize = small, cfgVariant = v } ]
  , t "số quân trên bàn == số nước đã đi" $
      and [ M.size (gsBoard g) == length (gsHistory g)
          | seed <- [1 .. 20], g <- randomPlay seed 80 (newGame smallCfg) ]
  , t "undo hết mọi nước -> về đúng ván mới" $
      and [ undoAll final == newGame smallCfg
          | seed <- [1 .. 20]
          , let final = last (randomPlay seed 80 (newGame smallCfg)) ]
  , t "lưu rồi nạp (encode/decode) cho lại đúng ván" $
      and [ decodeSave (encodeSave final) == Right final
          | seed <- [1 .. 30]
          , let final = last (randomPlay seed 60 (newGame smallCfg)) ]
  , t "tệp lưu sai luật bị từ chối"
      (either ("sai luật" `isInfixOf`) (const False)
         (decodeSave "caro-save 1\nsize: 15\nvariant: caro\ntime: unlimited\nmoves: h8 h8\n"))
  , t "tệp lưu hỏng cú pháp bị từ chối"
      (either ("không hợp lệ" `isInfixOf`) (const False) (decodeSave "xin chao"))
  , t "tệp lưu cỡ bàn sai bị từ chối"
      (either (const True) (const False)
         (decodeSave "caro-save 1\nsize: 3\nvariant: caro\ntime: unlimited\nmoves:\n"))
  ]
  where
    small    = either (error . show) id (mkSize 9)
    smallCfg = cfgV { cfgSize = small }
    ok g     = isWon (gsStatus g) == bruteWon (gsConfig g) (gsBoard g)
    undoAll g = either (const g) undoAll (undoMove g)

parserTests :: [Test]
parserTests =
  [ t ("đọc được: " ++ show inp) (fmap normalize (parseCommand inp) == Right out)
  | (inp, out) <-
      [ ("h8", CMove (Pos 7 7)), ("H8", CMove (Pos 7 7)), ("h 8", CMove (Pos 7 7))
      , ("8h", CMove (Pos 7 7)), ("8 h", CMove (Pos 7 7)), ("8 8", CMove (Pos 7 7))
      , ("8,8", CMove (Pos 7 7)), ("8, 8", CMove (Pos 7 7))
      , ("  a1  ", CMove (Pos 0 0)), ("o15", CMove (Pos 14 14))
      , ("move h8", CMove (Pos 7 7)), ("PLAY h8", CMove (Pos 7 7))
      , ("undo", CUndo), ("UNDO", CUndo), ("hint", CHint), ("show", CShow)
      , ("help", CHelp), ("?", CHelp), ("resign", CResign)
      , ("quit", CQuit), ("exit", CQuit)
      , ("save", CSave Nothing), ("save van 1.sav", CSave (Just "van 1.sav"))
      , ("load a.sav", CLoad "a.sav")
      , ("new", CNew Nothing), ("new 19", CNew (Just 19))
      ] ]
  ++
  [ t ("từ chối: " ++ show inp) (either (const True) (const False) (parseCommand inp))
  | inp <- ["", "   ", "xyz", "h", "8", "h8 h9", "h8x", "undo now", "load", "new x"
           , "1234567 1", "move", "h-8", "undoo"] ]
  ++
  [ t "thông báo lỗi chỉ đúng vị trí và gợi ý help"
      (case parseCommand "h8x" of
         Left e  -> let m = renderParseError "h8x" e
                    in "^" `isInfixOf` m && "help" `isInfixOf` m && "'x'" `isInfixOf` m
         Right _ -> False)
  , t "thông báo lỗi không còn chữ Parsec tiếng Anh"
      (all (\inp -> case parseCommand inp of
                      Left e  -> not (any (`isInfixOf` renderParseError inp e) ["digit", "space", "letter", "unexpected"])
                      Right _ -> True)
           ["xyz", "h8x", "load", "new x", "undo x", "8", ""])
  ]
  where
    normalize = id

protocolTests :: [Test]
protocolTests =
  [ t "gói tin khứ hồi (render rồi parse)"
      (all (\m -> parseMsg (renderMsg m) == Right m)
           [Hello 15 VietCaro, Hello 19 Freestyle, MsgMove (Pos 7 7), MsgMove (Pos 0 14), MsgResign, MsgBye])
  , t "chấp nhận CRLF và hoa/thường" (parseMsg "move 3 4\r" == Right (MsgMove (Pos 3 4)))
  , t "từ chối gói tin lạ" (all (either (const True) (const False) . parseMsg) ["", "HELLO", "MOVE 1", "FOO", "MOVE a b"])
  ]

aiTests :: [Test]
aiTests =
  [ t "thắng ngay nếu có thể (bốn quân mở)"
      (case moves cfgF (xWinning ["h8", "i8", "j8", "k8"]) of
         Right g -> all (\d -> bestMove d g `elem` [Just (at "g8"), Just (at "l8")]) [1, 2, 3]
         Left _  -> False)
  , t "chặn bốn quân của đối thủ khi không thể thắng"
      (case moves cfgV ["h8", "g8", "i8", "a1", "j8", "c1", "k8"] of
         Right g -> gsTurn g == O && all (\d -> bestMove d g == Just (at "l8")) [1, 2, 3]
         Left _  -> False)
  , t "bàn trống: đi giữa bàn" (bestMove 2 (newGame cfgV) == Just (Pos 7 7))
  , t "ván đã xong: không có nước đi" (bestMove 2 (resign X (newGame cfgV)) == Nothing)
  , t "nước đi của máy luôn hợp lệ (ván ngẫu nhiên)"
      (and [ maybe False (\p -> either (const False) (const True) (applyMove p g)) (bestMove 2 g)
           | seed <- [1 .. 8], g <- take 4 (reverse (randomPlay seed 24 (newGame cfgV))), gsStatus g == InProgress ])
  , t "Best: phép gộp có tính kết hợp và giao hoán"
      (let xs = [Best 3 (Pos 1 1), Best 3 (Pos 0 5), Best 9 (Pos 2 2), NoBest, Best (-4) (Pos 0 0)]
       in and [ (a <> b) <> c == a <> (b <> c) && a <> b == b <> a | a <- xs, b <- xs, c <- xs ])
  , tIO "song song == tuần tự (1, 2, 3, 8 luồng)" $ do
      let gs = [ g | Right g <- [moves cfgV ["h8", "i9", "h9", "i10", "h10"]
                               , moves cfgV ["h8", "i8", "i9", "g7", "j10", "k11"]
                               , Right (newGame cfgV)] ]
      rs <- forM gs $ \g -> forM [1, 2, 3, 8] $ \w -> do
              par <- parBestMove w 3 g
              pure (par == bestMove 3 g)
      pure (and (concat rs) && not (null gs))
  ]

-- | Kiểm thử lõi thuần 'step' (không cần IO, luồng hay thời gian).
sessionTests :: [Test]
sessionTests =
  [ t "gõ nước đi: đổi ván, tăng phiên bản, vẽ lại bàn"
      (let (s', fx) = step (EvLine "h8") pvp
       in sVersion s' == 1 && M.size (gsBoard (sGame s')) == 1 && any isShow fx)
  , t "nước đi sai: báo lỗi, ván không đổi"
      (let (s1, _)  = step (EvLine "h8") pvp
           (s2, fx) = step (EvLine "h8") s1
       in s2 == s1 && any (sayHas "đã có quân") fx)
  , t "lệnh sai cú pháp: báo lỗi có kiểu, ván không đổi"
      (let (s', fx) = step (EvLine "xyz") pvp in s' == pvp && any (sayHas "Không hiểu lệnh") fx)
  , t "dòng trống: bỏ qua" (let (s', fx) = step (EvLine "  ") pvp in s' == pvp && null fx)
  , t "undo (2 người): lùi một nước"
      (let (s1, _) = step (EvLine "h8") pvp
           (s2, _) = step (EvLine "undo") s1
       in M.null (gsBoard (sGame s2)) && gsTurn (sGame s2) == X)
  , t "đấu máy: sau nước của người, máy được giao việc đúng phiên bản"
      (let (s1, fx) = step (EvLine "h8") botX in sVersion s1 == 1 && any (isStartBot 1) fx)
  , t "đấu máy: nhận nước của máy thì đến lượt người"
      (let (s1, _)  = step (EvLine "h8") botX
           (s2, _)  = step (EvBotMove 1 (at "i9")) s1
       in gsTurn (sGame s2) == X && M.size (gsBoard (sGame s2)) == 2)
  , t "đấu máy: kết quả CŨ của máy (sau undo) bị bỏ qua — chống tranh chấp"
      (let (s1, _) = step (EvLine "h8") botX         -- máy nhận việc ở phiên bản 1
           (s2, _) = step (EvLine "undo") s1         -- người undo: phiên bản 2
           (s3, fx) = step (EvBotMove 1 (at "i9")) s2 -- máy báo kết quả phiên bản 1
       in s3 == s2 && null fx)
  , t "đấu máy: người cầm O thì máy đi trước"
      (let (_, fx) = startSession (newSession (VsBot O Easy) (newGame cfgV) "x.sav") in any (isStartBot 0) fx)
  , t "đấu máy: không đi thay máy"
      (let (s1, _) = step (EvLine "h8") botX
           (s2, fx) = step (EvLine "i9") s1
       in s2 == s1 && any (sayHas "Chưa đến lượt") fx)
  , t "mạng: chưa đến lượt thì không đi được"
      (let (s', fx) = step (EvLine "h8") (online O) in s' == online O && any (sayHas "Chưa đến lượt") fx)
  , t "mạng: đi xong gửi gói tin"
      (let (_, fx) = step (EvLine "h8") (online X) in any isSendMove fx)
  , t "mạng: nhận nước của đối thủ"
      (let (s', _) = step (EvNet (MsgMove (at "h8"))) (online O) in gsTurn (sGame s') == O && M.size (gsBoard (sGame s')) == 1)
  , t "mạng: gói tin sai lượt bị bỏ qua"
      (let (s', _) = step (EvNet (MsgMove (at "h8"))) (online X) in s' == online X)
  , t "mạng: không cho undo"
      (let (s', fx) = step (EvLine "undo") (online X) in s' == online X && any (sayHas "không dùng được") fx)
  , t "mạng: đối thủ đầu hàng thì mình thắng"
      (let (s', _) = step (EvNet MsgResign) (online X) in gsStatus (sGame s') == Won X Resignation)
  , t "đồng hồ: hết giờ thì thua"
      (let s0 = newSession PvP (newGame cfgV { cfgTime = PerMove 5 }) "x.sav"
           s5 = iterate (fst . step EvTick) s0 !! 5
       in gsStatus (sGame s5) == Won O Timeout)
  , t "đồng hồ: không chạy khi máy đang nghĩ"
      (let cfg = cfgV { cfgTime = PerMove 5 }
           (s1, _) = step (EvLine "h8") (newSession (VsBot X Easy) (newGame cfg) "x.sav")
           s9 = iterate (fst . step EvTick) s1 !! 9
       in gsStatus (sGame s9) == InProgress)
  , t "đồng hồ: tick thường không tăng phiên bản (không kích hoạt tự lưu liên tục)"
      (let s0 = newSession PvP (newGame cfgV { cfgTime = PerMove 30 }) "x.sav"
       in sVersion (fst (step EvTick s0)) == 0)
  , t "new đổi cỡ bàn; cỡ sai bị báo lỗi"
      (let (s1, _) = step (EvLine "new 19") pvp
           (s2, fx) = step (EvLine "new 3") pvp
       in unSize (cfgSize (gsConfig (sGame s1))) == 19 && s2 == pvp && any (sayHas "Cỡ bàn") fx)
  , t "load: tệp lỗi chỉ báo lỗi, ván giữ nguyên"
      (let (s', fx) = step (EvLoaded "x" (Left "hỏng")) pvp in s' == pvp && any (sayHas "Không nạp được") fx)
  , t "quit: lưu (nếu đã có nước) rồi dừng"
      (let (s1, _) = step (EvLine "h8") pvp
           (s2, fx) = step (EvLine "quit") s1
       in sDone s2 && any isSave fx && any isStop fx)
  , t "quit ván trống: không ghi đè tệp cũ"
      (let (_, fx) = step (EvLine "quit") pvp in not (any isSave fx) && any isStop fx)
  ]
  where
    pvp      = newSession PvP (newGame cfgV) "x.sav"
    botX     = newSession (VsBot X Easy) (newGame cfgV) "x.sav"
    online m = newSession (Online m) (newGame cfgV) "x.sav"
    isShow (ShowBoard _) = True
    isShow _             = False
    isStartBot v (StartBot v' _ _) = v == v'
    isStartBot _ _                 = False
    isSendMove (Send (MsgMove _)) = True
    isSendMove _                  = False
    isSave (SaveTo _ _) = True
    isSave _            = False
    isStop Stop = True
    isStop _    = False
    sayHas s (Say m)  = s `isInfixOf` m
    sayHas s (Warn m) = s `isInfixOf` m
    sayHas _ _       = False

cliTests :: [Test]
cliTests =
  [ t "tham số đầy đủ"
      (parseArgs ["--size", "19", "--variant", "freestyle", "--time", "30", "--bot", "hard", "--play", "O"]
         == Right defaultOptions { oSize = 19, oVariant = Freestyle, oTime = PerMove 30
                                 , oMode = VsMachine Hard, oHuman = O })
  , t "giao diện đồ họa: --gui --port --no-browser"
      (parseArgs ["--gui", "--port", "9000", "--no-browser", "--bot", "normal"]
         == Right defaultOptions { oGui = True, oPort = 9000, oBrowser = False, oMode = VsMachine Normal })
  , t "--join MÁY:CỔNG" (fmap oMode (parseArgs ["--join", "localhost:5000"]) == Right (JoinMode "localhost" 5000))
  , t "không chọn hai chế độ cùng lúc" (either (const True) (const False) (parseArgs ["--bot", "easy", "--host", "5000"]))
  , t "tham số sai bị từ chối"
      (all (either (const True) (const False) . parseArgs)
           [["--size"], ["--size", "x"], ["--bot", "god"], ["--join", "localhost"], ["--lạ"]])
  ]

-- | Chế độ --ascii (cho console Windows hiện sai UTF-8).
asciiTests :: [Test]
asciiTests =
  [ t "bỏ dấu tiếng Việt đúng"
      (unaccent "Cờ caro bằng Haskell" == "Co caro bang Haskell"
        && unaccent "Đang chờ đối thủ kết nối" == "Dang cho doi thu ket noi"
        && unaccent "ĐƯỜNG THẮNG ÔỒ Ở" == "DUONG THANG OO O")
  , t "mọi chữ có dấu tiếng Việt (hoa và thường) đều thành ASCII"
      (let vn = "àáạảãâầấậẩẫăằắặẳẵèéẹẻẽêềếệểễìíịỉĩòóọỏõôồốộổỗơờớợởỡùúụủũưừứựửữỳýỵỷỹđ"
       in all isAscii (unaccent vn) && all isAscii (unaccent (map toUpper vn)))
  , t "dấu câu Unicode và ký tự lạ cũng thành ASCII"
      (unaccent "a…b – c “d”" == "a...b - c \"d\"" && unaccent "日本" == "??")
  , t "ASCII giữ nguyên"
      (let s = "h8 [X] <O> > 12,3 --size 15 \n" in unaccent s == s)
  , t "mọi thông báo của chương trình chuyển được thành ASCII thuần"
      (all (all isAscii . unaccent)
        [ usage, helpText, pretty GameOver, pretty (Occupied (Pos 7 7)), pretty (OutOfBounds (Pos 0 0))
        , pretty NothingToUndo, pretty (BadSize 3), pretty (BadTime 1), pretty NotYourTurn
        , pretty (Unsupported "undo"), pretty (Won X Timeout), pretty (Won O Resignation)
        , pretty Draw, pretty VietCaro, pretty Freestyle, modeText (VsBot X Hard), modeText (Online O)
        , either id (const "") (decodeSave "xin chao") ])
  , t "--ascii là tùy chọn dòng lệnh" (fmap oAscii (parseArgs ["--ascii"]) == Right True
        && fmap oAscii (parseArgs []) == Right False)
  ]

-- | Kiểm thử đồng thời: chạy thật 'runLoop' với hàng đợi STM, các luồng nền, tệp.
concurrencyTests :: [Test]
concurrencyTests =
  [ tIO "ván trọn vẹn qua hàng đợi sự kiện: X thắng, tệp lưu nạp lại đúng" $ do
      path <- tmpFile "caro-test-game.sav"
      (env, outRef) <- mkEnv (newSession PvP (newGame cfgV) path)
      forM_ (xWinning ["h8", "i8", "j8", "k8", "l8"] ++ ["quit"]) (post env . EvLine)
      runLoop env
      out <- concat . reverse <$> readIORef outRef
      loaded <- loadGame path
      cleanup path
      pure ("X thắng" `isInfixOf` out && fmap (isWon . gsStatus) loaded == Right True)
  , tIO "16 luồng cùng đẩy nước đi: không mất cập nhật (STM + một luồng xử lý)" $ do
      path <- tmpFile "caro-test-race.sav"
      (env, _) <- mkEnv (newSession PvP (newGame cfgV) path)
      let cells = [ Pos (2 * i) (2 * j) | i <- [0 .. 3], j <- [0 .. 3] ]
      forM_ cells $ \p -> forkIO (post env (EvLine (posText p)))
      -- luồng chính xử lý đúng 16 sự kiện (do 16 luồng khác nhau đẩy vào)
      runLoopN 16 env
      s <- readTVarIO (envSession env)
      cleanup path
      pure (M.size (gsBoard (sGame s)) == 16 && sVersion s == 16)
  , tIO "luồng tự lưu: ván đổi thì tệp tự được ghi (không cần quit)" $ do
      path <- tmpFile "caro-test-autosave.sav"
      cleanup path
      (env, _) <- mkEnv (newSession PvP (newGame cfgV) path)
      loopT <- forkIO (runLoop env)
      saverT <- forkIO (autosaveThread env)
      forM_ ["h8", "i9", "h9"] (post env . EvLine)
      ok <- waitFor 5000 $ do
        ex <- doesFileExist path
        if not ex then pure False else do
          r <- loadGame path
          pure (fmap (length . gsHistory) r == Right 3)
      killThread loopT >> killThread saverT
      cleanup path
      pure ok
  , tIO "đồng hồ + máy đồng thời: máy trả lời trong lúc đồng hồ chạy" $ do
      path <- tmpFile "caro-test-bot.sav"
      let cfg = cfgV { cfgTime = PerMove 60 }
      (env, _) <- mkEnv (newSession (VsBot X Normal) (newGame cfg) path)
      loopT  <- forkIO (runLoop env)
      clockT <- forkIO (clockThread env)
      post env (EvLine "h8")
      ok <- waitFor 8000 $ do
        s <- readTVarIO (envSession env)
        pure (length (gsHistory (sGame s)) >= 2)
      killThread loopT >> killThread clockT
      cleanup path
      pure ok
  ]
  where
    tmpFile n = (\d -> d ++ "/" ++ n) <$> getTemporaryDirectory
    cleanup p = forM_ [p, p ++ ".tmp"] $ \f -> do
      e <- doesFileExist f
      when e (removeFile f)
    mkEnv s = do
      ref <- newIORef []
      env <- newEnv (\str -> modifyIORef' ref (str :)) Nothing 2 s
      pure (env, ref)
    -- xử lý đúng n sự kiện rồi dừng (dùng cho kiểm thử đua tranh)
    runLoopN :: Int -> Env -> IO ()
    runLoopN 0 _ = pure ()
    runLoopN n env = do
      ev <- atomically (readTQueue (envQueue env))
      fx <- atomically $ do
        s <- readTVar (envSession env)
        let (s', effs) = step ev s
        writeTVar (envSession env) s'
        pure effs
      runEffects env fx
      runLoopN (n - 1) env

-- | Chờ tối đa @ms@ mili-giây cho đến khi điều kiện đúng.
waitFor :: Int -> IO Bool -> IO Bool
waitFor ms cond
  | ms <= 0   = pure False
  | otherwise = do
      ok <- cond
      if ok then pure True else threadDelay 50000 >> waitFor (ms - 50) cond

-- ---------------------------------------------------------------------------
-- Giao diện đồ họa

hostHdr :: Int -> (String, String)
hostHdr port = ("Host", "localhost:" ++ show port)

mkReq :: String -> [(String, String)] -> Request
mkReq target hs = case parseRequestHead
    (target ++ " HTTP/1.1\r\n" ++ concat [ k ++ ": " ++ v ++ "\r\n" | (k, v) <- hs ] ++ "\r\n") of
  Just r  -> r
  Nothing -> error ("yêu cầu sai trong kiểm thử: " ++ target)

guiPureTests :: [Test]
guiPureTests =
  [ t "JSON: thoát ký tự đặc biệt, giữ nguyên tiếng Việt"
      (render (JStr "a\"b\\c\nd\t\1 ô") == "\"a\\\"b\\\\c\\nd\\t\\u0001 ô\"")
  , t "JSON: đối tượng, mảng lồng nhau, null, bool, số"
      (render (JObj [("a", JArr [JNum 1, JNum (-2), JNull]), ("b", JBool True), ("c", JObj [])])
         == "{\"a\":[1,-2,null],\"b\":true,\"c\":{}}")
  , t "đọc phần đầu HTTP: phương thức, đường dẫn, query, header (tên viết thường)"
      (parseRequestHead "GET /api/state?stamp=1-2&x=y HTTP/1.1\r\nHost: localhost:80\r\nOrigin:  http://a \r\n"
         == Just (Request "GET" "/api/state" [("stamp", "1-2"), ("x", "y")]
                          [("host", "localhost:80"), ("origin", "http://a")]))
  , t "đọc phần đầu HTTP: rác bị từ chối"
      (all ((== Nothing) . parseRequestHead) ["", "xin chao", "GET /", "GET / FTP/1.0"])
  , t "định tuyến: các thao tác hợp lệ"
      (let ok = [hostHdr 8123]
           r m tgt = route 8123 (mkReq (m ++ " " ++ tgt) ok)
       in r "GET" "/" == RIndex
          && r "GET" "/api/state?stamp=abc" == RState "abc"
          && r "GET" "/api/state" == RState ""
          && r "POST" "/api/move?r=7&c=7" == RAct (AMove (Pos 7 7))
          && r "POST" "/api/new?size=19" == RAct (ANew 19)
          && map (r "POST") ["/api/undo", "/api/hint", "/api/resign", "/api/save", "/api/load", "/api/quit"]
               == map RAct [AUndo, AHint, AResign, ASave, ALoad, AQuit])
  , t "định tuyến: tham số xấu bị từ chối (400)"
      (let r tgt = route 8123 (mkReq ("POST " ++ tgt) [hostHdr 8123])
       in all (\tgt -> case r tgt of { RRefuse 400 _ -> True; _ -> False })
            ["/api/move?r=abc&c=1", "/api/move?r=1", "/api/move?r=-1&c=0", "/api/move?r=0&c=99", "/api/new?size=x", "/api/new"])
  , t "định tuyến: Host lạ hoặc thiếu bị chặn (403) — chống DNS rebinding"
      (and [ case route 8123 (mkReq "GET /" hs) of { RRefuse 403 _ -> True; _ -> False }
           | hs <- [[], [("Host", "evil.com")], [("Host", "localhost:9999")], [("Host", "localhost")]] ])
  , t "định tuyến: Origin lạ bị chặn (403), Origin của chính trang thì được"
      (case ( route 8123 (mkReq "POST /api/undo" [hostHdr 8123, ("Origin", "http://evil.com")])
            , route 8123 (mkReq "POST /api/undo" [hostHdr 8123, ("Origin", "http://localhost:8123")])
            , route 8123 (mkReq "POST /api/undo" [("Host", "127.0.0.1:8123"), ("Origin", "http://127.0.0.1:8123")]) ) of
         (RRefuse 403 _, RAct AUndo, RAct AUndo) -> True
         _                                       -> False)
  , t "định tuyến: đường dẫn lạ (404), phương thức lạ (405)"
      (case ( route 8123 (mkReq "GET /etc/passwd" [hostHdr 8123])
            , route 8123 (mkReq "PUT /api/undo" [hostHdr 8123])
            , route 8123 (mkReq "GET /api/undo" [hostHdr 8123]) ) of
         (RRefuse 404 _, RRefuse 405 _, RRefuse 404 _) -> True
         _                                             -> False)
  , t "không có đường nào truyền tên tệp tùy ý: save/load dùng đúng đường dẫn đã cấu hình"
      (let s = newSession PvP (newGame cfgV) "van.sav"
           line a = case actionEvent s a of { EvLine l -> l; _ -> "?" }
       in line ASave == "save" && line ALoad == "load van.sav" && line (AMove (Pos 7 7)) == "h8"
            && line (ANew 19) == "new 19" && line AQuit == "quit")
  , t "nhật ký thông báo: mới nhất đứng đầu, id tăng dần, giữ tối đa 40"
      (let lg = foldl (\l i -> appendMsg SevInfo (show i) l) emptyLog [1 .. 60 :: Int]
       in length (mlItems lg) == 40 && gmText (head (mlItems lg)) == "60"
            && gmId (head (mlItems lg)) == 60 && gmText (last (mlItems lg)) == "21")
  , t "trạng thái JSON: nước đi, lượt, quyền đi"
      (let (s1, _) = step (EvLine "h8") (newSession PvP (newGame cfgV) "x.sav")
           j = render (stateJson s1 [])
       in all (`isInfixOf` j) ["\"moves\":[[7,7,\"X\"]]", "\"turn\":\"O\"", "\"canMove\":true", "\"mode\":\"pvp\"", "\"size\":15"])
  , t "trạng thái JSON: máy đang nghĩ thì người không được đi"
      (let (s1, _) = step (EvLine "h8") (newSession (VsBot X Easy) (newGame cfgV) "x.sav")
           j = render (stateJson s1 [])
       in "\"thinking\":true" `isInfixOf` j && "\"canMove\":false" `isInfixOf` j && "\"me\":\"X\"" `isInfixOf` j)
  , t "trạng thái JSON: ván thắng có dãy quân, không còn được đi"
      (case moves cfgV (xWinning ["h8", "i8", "j8", "k8", "l8"]) of
         Right g -> let j = render (stateJson (newSession PvP g "x.sav") [])
                    in all (`isInfixOf` j) ["\"kind\":\"won\"", "\"winner\":\"X\"", "\"reason\":\"five\"", "[7,7],[7,8],[7,9],[7,10],[7,11]", "\"canMove\":false"]
         Left _  -> False)
  , t "trạng thái JSON: gợi ý hiện ra rồi biến mất khi ván đổi"
      (let (s1, _) = step (EvLine "hint") (newSession PvP (newGame cfgV) "x.sav")
           (s2, _) = step (EvLine "h8") s1
       in "\"hint\":[7,7]" `isInfixOf` render (stateJson s1 []) && "\"hint\":null" `isInfixOf` render (stateJson s2 []))
  , t "dấu vân tay đổi khi ván đổi, khi có thông báo mới, khi đồng hồ chạy"
      (let s0 = newSession PvP (newGame cfgV { cfgTime = PerMove 30 }) "x.sav"
           (s1, _) = step (EvLine "h8") s0
           s2 = fst (step EvTick s1)
       in stampOf s0 [] /= stampOf s1 []
            && stampOf s1 [] /= stampOf s2 []
            && stampOf s1 [] /= stampOf s1 [GuiMsg 1 SevInfo "x"])
  ]

-- | Máy chủ HTTP thật (cổng do hệ điều hành chọn) + trình duyệt giả bằng socket.
guiHttpTests :: [Test]
guiHttpTests =
  [ tIO "HTTP thật: trang chủ, đặt quân, nước sai bị báo lỗi, chặn Host/Origin lạ" $ withGui $ \port -> do
      (c0, page) <- http port "GET" "/" [hostHdr port]
      (c1, _)    <- http port "POST" "/api/move?r=7&c=7" [hostHdr port, ("Origin", "http://localhost:" ++ show port)]
      okMove <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure ("\"moves\":[[7,7,\"X\"]]" `isInfixOf` b)
      _ <- http port "POST" "/api/move?r=7&c=7" [hostHdr port]
      okWarn <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure ("\"level\":\"warning\"" `isInfixOf` b && "đã có quân" `isInfixOf` b)
      (c2, _) <- http port "GET" "/" [("Host", "evil.com")]
      (c3, _) <- http port "POST" "/api/undo" [hostHdr port, ("Origin", "http://evil.com")]
      (c4, _) <- http port "POST" "/api/move?r=zz&c=1" [hostHdr port]
      pure (c0 == 200 && "<canvas" `isInfixOf` page && "Cờ caro" `isInfixOf` page
              && c1 == 200 && okMove && okWarn && c2 == 403 && c3 == 403 && c4 == 400)
  , tIO "HTTP thật: long-poll chờ đến khi ván đổi (không thăm dò, không chờ đủ 20 giây)" $ withGui $ \port -> do
      (_, b0) <- getState port ""
      let st0 = stampOfJson b0
      _ <- forkIO (threadDelay 400000 >> void (http port "POST" "/api/move?r=3&c=3" [hostHdr port]))
      t0 <- getMonotonicMs
      (_, b1) <- getState port st0
      t1 <- getMonotonicMs
      pure (stampOfJson b1 /= st0 && "[3,3,\"X\"]" `isInfixOf` b1 && t1 - t0 < 5000)
  , tIO "HTTP thật: undo, hint, new qua giao diện đi qua đúng luật" $ withGui $ \port -> do
      forM_ [("POST", "/api/move?r=7&c=7"), ("POST", "/api/move?r=8&c=8")] $ \(m, u) -> http port m u [hostHdr port]
      _ <- http port "POST" "/api/undo" [hostHdr port]
      okUndo <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure ("\"moves\":[[7,7,\"X\"]]" `isInfixOf` b && "\"turn\":\"O\"" `isInfixOf` b)
      _ <- http port "POST" "/api/hint" [hostHdr port]
      okHint <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure (not ("\"hint\":null" `isInfixOf` b))
      _ <- http port "POST" "/api/new?size=3" [hostHdr port]
      okBad <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure ("Cỡ bàn" `isInfixOf` b && "\"size\":15" `isInfixOf` b)
      _ <- http port "POST" "/api/new?size=19" [hostHdr port]
      okNew <- waitFor 3000 $ do
        (_, b) <- getState port ""
        pure ("\"size\":19" `isInfixOf` b && "\"moves\":[]" `isInfixOf` b)
      pure (okUndo && okHint && okBad && okNew)
  ]
  where
    stampOfJson b = takeWhile (/= '"') (drop (length "{\"stamp\":\"") b)
    getMonotonicMs = (round . (* 1000)) <$> getMonotonicTime :: IO Int

-- | Khởi động một giao diện đồ họa thật trên cổng tự chọn, chạy @k port@, rồi dọn dẹp.
withGui :: (Int -> IO Bool) -> IO Bool
withGui k = do
  dir <- getTemporaryDirectory
  let path = dir ++ "/caro-test-gui.sav"
  lg <- newTVarIO emptyLog
  let (s0, fx0) = startSession (newSession PvP (newGame cfgV) path)
  env <- newEnvWith (guiOutput lg (\_ -> pure ())) Nothing 2 s0
  (sock, port) <- bindGui 0
  runEffects env fx0
  tids <- mapM forkIO [runLoop env, serveGui (Gui env lg port) sock]
  k port `finally` do
    mapM_ killThread tids
    close sock
    forM_ [path, path ++ ".tmp"] $ \f -> do { e <- doesFileExist f; when e (removeFile f) }

-- | Một yêu cầu HTTP thô; trả về (mã trạng thái, thân phản hồi).
http :: Int -> String -> String -> [(String, String)] -> IO (Int, String)
http port method target hdrs = do
  let req = method ++ " " ++ target ++ " HTTP/1.1\r\n"
              ++ concat [ k ++ ": " ++ v ++ "\r\n" | (k, v) <- hdrs ] ++ "\r\n"
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    sendAll sock (BC.pack req)
    raw <- recvAll sock
    let (hd, rest) = B.breakSubstring (BC.pack "\r\n\r\n") raw
        code       = case words (BC.unpack hd) of { (_ : c : _) -> read c; _ -> 0 }
    pure (code, T.unpack (TE.decodeUtf8 (B.drop 4 rest)))
  where
    recvAll sock = go B.empty
      where go acc = do
              chunk <- recv sock 4096
              if B.null chunk then pure acc else go (acc <> chunk)

getState :: Int -> String -> IO (Int, String)
getState port stamp = http port "GET" ("/api/state?stamp=" ++ stamp) [hostHdr port]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdout utf8
  rs <- sequence
    [ section "Luật chơi (hàm thuần)" ruleTests
    , section "Tính chất (property) với ván ngẫu nhiên" propertyTests
    , section "Parser lệnh" parserTests
    , section "Giao thức mạng" protocolTests
    , section "Máy đánh cờ & song song" aiTests
    , section "Lõi phiên chơi (step thuần)" sessionTests
    , section "Tham số dòng lệnh" cliTests
    , section "Chế độ --ascii (console không hiện UTF-8)" asciiTests
    , section "Giao diện đồ họa (phần thuần: JSON, HTTP, định tuyến, bảo mật)" guiPureTests
    , section "Giao diện đồ họa (máy chủ HTTP thật)" guiHttpTests
    , section "Đồng thời, STM, tệp" concurrencyTests
    ]
  let results = concat rs
      failed  = length (filter not results)
  putStrLn ""
  putStrLn (show (length results - failed) ++ "/" ++ show (length results) ++ " kiểm thử đạt")
  when (failed > 0) exitFailure
