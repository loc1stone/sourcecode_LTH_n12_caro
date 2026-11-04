-- | Đọc tham số dòng lệnh thành giá trị có kiểu ('Either' cho lỗi).
module Caro.Cli (Options (..), PlayMode (..), defaultOptions, parseArgs, usage) where

import Data.Char (toLower)

import Caro.AI (Difficulty (..))
import Caro.Types

data PlayMode
  = Local
  | VsMachine Difficulty
  | HostMode Int
  | JoinMode String Int
  | BenchMode
  deriving (Eq, Show)

data Options = Options
  { oSize    :: Int
  , oVariant :: Variant
  , oTime    :: TimeControl
  , oMode    :: PlayMode
  , oHuman   :: Player      -- ^ quân của người khi đấu với máy
  , oSave    :: FilePath
  , oResume  :: Bool
  , oHelp    :: Bool
  , oGui     :: Bool        -- ^ giao diện đồ họa trên trình duyệt
  , oPort    :: Int         -- ^ cổng của giao diện đồ họa
  , oBrowser :: Bool        -- ^ tự mở trình duyệt
  , oAscii   :: Bool        -- ^ in tiếng Việt không dấu (console không hiện UTF-8)
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { oSize = 15, oVariant = VietCaro, oTime = Unlimited, oMode = Local
  , oHuman = X, oSave = "caro.sav", oResume = False, oHelp = False
  , oGui = False, oPort = 8123, oBrowser = True, oAscii = False }

usage :: String
usage = unlines
  [ "Cờ caro bằng Haskell"
  , ""
  , "Cách dùng: caro [tùy chọn]"
  , "  --size N            cỡ bàn " ++ show minSize ++ ".." ++ show maxSize ++ " (mặc định 15)"
  , "  --variant caro|freestyle   luật chơi (mặc định caro: dãy bị chặn 2 đầu không thắng)"
  , "  --time GIÂY         giới hạn thời gian mỗi nước, 5..600 (mặc định: không giới hạn)"
  , "  --bot easy|normal|hard     đấu với máy"
  , "  --play X|O          quân của bạn khi đấu với máy (mặc định X, đi trước)"
  , "  --host CỔNG         tạo phòng chơi qua mạng (bạn cầm X)"
  , "  --join MÁY:CỔNG     vào phòng của người khác (bạn cầm O), ví dụ localhost:5000"
  , "  --save TỆP          tệp tự động lưu (mặc định caro.sav)"
  , "  --resume            nạp lại ván từ tệp tự động lưu"
  , "  --gui               giao diện đồ họa trên trình duyệt (dùng được cùng --bot/--host/--join)"
  , "  --port N            cổng của giao diện đồ họa (mặc định 8123)"
  , "  --no-browser        không tự mở trình duyệt, chỉ in địa chỉ"
  , "  --ascii             in tiếng Việt KHÔNG DẤU (dùng khi console Windows hiện chữ bị lỗi)"
  , "  --bench             so sánh thời gian tính nước đi với 1/2/4 luồng"
  , "  --help              hiện hướng dẫn này" ]

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go o [] = Right o
    go o ("--help" : r)    = go o { oHelp = True } r
    go o ("--resume" : r)  = go o { oResume = True } r
    go o ("--gui" : r)     = go o { oGui = True } r
    go o ("--ascii" : r)   = go o { oAscii = True } r
    go o ("--no-browser" : r) = go o { oBrowser = False } r
    go o ("--port" : v : r)   = int "--port" v >>= \n -> go o { oPort = n } r
    go o ("--bench" : r)   = setMode o BenchMode >>= \o' -> go o' r
    go o ("--size" : v : r)    = int "--size" v >>= \n -> go o { oSize = n } r
    go o ("--time" : v : r)    = int "--time" v >>= \n -> go o { oTime = PerMove n } r
    go o ("--save" : v : r)    = go o { oSave = v } r
    go o ("--variant" : v : r) = case map toLower v of
      "caro"      -> go o { oVariant = VietCaro } r
      "freestyle" -> go o { oVariant = Freestyle } r
      _           -> Left ("Luật không hợp lệ: " ++ v ++ " (dùng caro hoặc freestyle)")
    go o ("--play" : v : r) = case map toLower v of
      "x" -> go o { oHuman = X } r
      "o" -> go o { oHuman = O } r
      _   -> Left ("Quân không hợp lệ: " ++ v ++ " (dùng X hoặc O)")
    go o ("--bot" : v : r) = case map toLower v of
      "easy"   -> setMode o (VsMachine Easy)   >>= \o' -> go o' r
      "normal" -> setMode o (VsMachine Normal) >>= \o' -> go o' r
      "hard"   -> setMode o (VsMachine Hard)   >>= \o' -> go o' r
      _        -> Left ("Mức máy không hợp lệ: " ++ v ++ " (dùng easy, normal hoặc hard)")
    go o ("--host" : v : r) = int "--host" v >>= \n -> setMode o (HostMode n) >>= \o' -> go o' r
    go o ("--join" : v : r) = do
      (h, p) <- hostPort v
      o' <- setMode o (JoinMode h p)
      go o' r
    go _ (a : _) = Left ("Tham số không hợp lệ: " ++ a)

    setMode o m
      | oMode o == Local = Right o { oMode = m }
      | otherwise        = Left "Chỉ chọn một chế độ: --bot, --host, --join hoặc --bench."

    int name v = case reads v of
      [(n, "")] -> Right n
      _         -> Left (name ++ " cần một số nguyên, nhận '" ++ v ++ "'")

    hostPort v = case break (== ':') (reverse v) of
      (revPort, ':' : revHost) | not (null revHost) ->
        int "--join" (reverse revPort) >>= \p -> Right (reverse revHost, p)
      _ -> Left "--join cần dạng MÁY:CỔNG, ví dụ localhost:5000"
