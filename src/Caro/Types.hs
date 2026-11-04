-- | Kiểu dữ liệu đại số (ADT) mô hình hóa miền bài toán cờ caro.
--
-- Ý tưởng chính: dùng kiểu để loại trừ trạng thái sai
--
--   * 'Size' và 'Config' chỉ tạo được qua hàm kiểm tra ('mkSize', 'mkConfig'),
--     nên không thể có bàn cờ cỡ 0 hay giới hạn thời gian âm.
--   * 'Status' tách rõ "đang chơi / có người thắng (kèm lý do) / hòa".
--   * 'MoveError' biểu diễn lỗi nước đi như một giá trị, không dùng ngoại lệ.
module Caro.Types
  ( Player (..), other
  , Pos (..), addPos, negPos
  , Size, mkSize, unSize, minSize, maxSize
  , Variant (..), variantKey
  , TimeControl (..), timeKey
  , Config (..), ConfigError (..), mkConfig, defaultConfig
  , Board, Move (..)
  , WinReason (..), Status (..), MoveError (..)
  , GameState (..)
  , winLength
  , colLetter, posText
  ) where

import Data.Map.Strict (Map)

-- | Hai người chơi. X luôn đi trước.
data Player = X | O
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

other :: Player -> Player
other X = O
other O = X

-- | Tọa độ 0-based: (hàng, cột).
data Pos = Pos { posRow :: !Int, posCol :: !Int }
  deriving (Eq, Ord, Show)

addPos :: Pos -> Pos -> Pos
addPos (Pos a b) (Pos c d) = Pos (a + c) (b + d)

negPos :: Pos -> Pos
negPos (Pos a b) = Pos (negate a) (negate b)

-- | Cỡ bàn cờ. Hàm tạo không được xuất ra ngoài: chỉ 'mkSize' mới tạo được.
newtype Size = Size Int
  deriving (Eq, Ord, Show)

minSize, maxSize :: Int
minSize = 9
maxSize = 26   -- đủ chữ cái a..z cho tên cột

mkSize :: Int -> Either ConfigError Size
mkSize n
  | n < minSize || n > maxSize = Left (BadSize n)
  | otherwise                  = Right (Size n)

unSize :: Size -> Int
unSize (Size n) = n

-- | Luật chơi.
--
--   * 'Freestyle': đủ 5 quân liên tiếp (hoặc hơn) là thắng.
--   * 'VietCaro': như trên, nhưng nếu dãy bị chặn cả hai đầu (bởi quân đối
--     thủ hoặc mép bàn) thì không tính thắng — luật caro phổ biến ở Việt Nam.
data Variant = Freestyle | VietCaro
  deriving (Eq, Show, Enum, Bounded)

-- | Tên dùng trong tệp lưu / giao thức mạng / tham số dòng lệnh.
variantKey :: Variant -> String
variantKey Freestyle = "freestyle"
variantKey VietCaro  = "caro"

data TimeControl = Unlimited | PerMove !Int   -- ^ số giây cho mỗi nước
  deriving (Eq, Show)

timeKey :: TimeControl -> String
timeKey Unlimited   = "unlimited"
timeKey (PerMove s) = show s

data Config = Config
  { cfgSize    :: !Size
  , cfgVariant :: !Variant
  , cfgTime    :: !TimeControl
  } deriving (Eq, Show)

data ConfigError = BadSize Int | BadTime Int
  deriving (Eq, Show)

-- | Hàm tạo có kiểm tra ("smart constructor").
mkConfig :: Int -> Variant -> TimeControl -> Either ConfigError Config
mkConfig n v t = do
  sz <- mkSize n
  t' <- case t of
          PerMove s | s < 5 || s > 600 -> Left (BadTime s)
          _                            -> Right t
  pure (Config sz v t')

defaultConfig :: Config
defaultConfig = Config (Size 15) VietCaro Unlimited

-- | Bàn cờ thưa: chỉ lưu các ô đã có quân.
type Board = Map Pos Player

data Move = Move { mvPlayer :: !Player, mvPos :: !Pos }
  deriving (Eq, Show)

data WinReason
  = FiveInRow [Pos]   -- ^ dãy quân liên tiếp tạo nên chiến thắng
  | Timeout           -- ^ đối thủ hết giờ
  | Resignation       -- ^ đối thủ đầu hàng
  deriving (Eq, Show)

data Status = InProgress | Won Player WinReason | Draw
  deriving (Eq, Show)

data MoveError
  = GameOver
  | OutOfBounds Pos
  | Occupied Pos
  | NothingToUndo
  deriving (Eq, Show)

data GameState = GameState
  { gsConfig  :: !Config
  , gsBoard   :: !Board
  , gsTurn    :: !Player
  , gsHistory :: [Move]     -- ^ nước mới nhất đứng đầu
  , gsStatus  :: !Status
  , gsClock   :: !Int       -- ^ giây còn lại của người đang đến lượt (0 nếu không giới hạn)
  } deriving (Eq, Show)

-- | Số quân liên tiếp cần để thắng.
winLength :: Int
winLength = 5

-- | Cột 0 -> 'a', cột 1 -> 'b', ...
colLetter :: Int -> Char
colLetter c = toEnum (fromEnum 'a' + c)

-- | Tọa độ dạng người dùng: cột chữ + hàng số (1-based), ví dụ @h8@.
posText :: Pos -> String
posText (Pos r c) = colLetter c : show (r + 1)
