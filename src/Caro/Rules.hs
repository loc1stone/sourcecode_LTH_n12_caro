-- | Toàn bộ quy tắc trò chơi dưới dạng HÀM THUẦN (không IO).
-- Mọi hàm ở đây có thể kiểm thử trực tiếp, không cần giao diện hay luồng.
module Caro.Rules
  ( newGame, clockLimit
  , applyMove, undoMove, resign, tick, replay
  , inBounds, legalMoves
  , directions, walk, LineInfo (..), lineInfo, winningLine
  ) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)

import Caro.Types

clockLimit :: Config -> Int
clockLimit cfg = case cfgTime cfg of
  Unlimited -> 0
  PerMove s -> s

newGame :: Config -> GameState
newGame cfg = GameState
  { gsConfig  = cfg
  , gsBoard   = M.empty
  , gsTurn    = X
  , gsHistory = []
  , gsStatus  = InProgress
  , gsClock   = clockLimit cfg
  }

inBounds :: Size -> Pos -> Bool
inBounds sz (Pos r c) = r >= 0 && c >= 0 && r < n && c < n
  where n = unSize sz

-- | Bốn hướng cần xét: ngang, dọc, chéo xuôi, chéo ngược.
directions :: [Pos]
directions = [Pos 0 1, Pos 1 0, Pos 1 1, Pos 1 (-1)]

-- | Các quân LIÊN TIẾP của người chơi @pl@ đi từ @from@ (không tính @from@)
-- theo hướng @d@.
--
-- ĐÁNH GIÁ LƯỜI: @iterate (addPos d) from@ là danh sách VÔ HẠN; nhờ lười,
-- 'takeWhile' chỉ duyệt đúng đến khi gặp ô trống / quân đối thủ / ngoài bàn.
walk :: Board -> Player -> Pos -> Pos -> [Pos]
walk b pl from d =
  takeWhile (\q -> M.lookup q b == Just pl) (drop 1 (iterate (addPos d) from))

-- | Thông tin một dãy quân đi qua một ô: các quân, và hai đầu còn trống không.
data LineInfo = LineInfo
  { liStones    :: [Pos]
  , liOpenBack  :: !Bool
  , liOpenFront :: !Bool
  } deriving (Eq, Show)

-- | Giả định ô @p@ đã có quân @pl@ trong bàn @b@.
lineInfo :: Size -> Board -> Player -> Pos -> Pos -> LineInfo
lineInfo sz b pl p d = LineInfo
  { liStones    = reverse back ++ [p] ++ fwd
  , liOpenBack  = open (addPos (negPos d) (lastOr p back))
  , liOpenFront = open (addPos d (lastOr p fwd))
  }
  where
    back = walk b pl p (negPos d)
    fwd  = walk b pl p d
    open q = inBounds sz q && M.notMember q b
    lastOr dflt [] = dflt
    lastOr _ xs    = last xs

-- | Dãy này có đủ điều kiện thắng theo luật không?
isWinning :: Variant -> LineInfo -> Bool
isWinning v li =
  length (liStones li) >= winLength
    && (v == Freestyle || liOpenBack li || liOpenFront li)

-- | Sau khi @pl@ vừa đặt quân ở @p@: có dãy thắng đi qua @p@ không?
winningLine :: Config -> Board -> Player -> Pos -> Maybe [Pos]
winningLine cfg b pl p = listToMaybe
  [ liStones li
  | d <- directions
  , let li = lineInfo (cfgSize cfg) b pl p d
  , isWinning (cfgVariant cfg) li
  ]

statusAfter :: Config -> Board -> Player -> Pos -> Status
statusAfter cfg b pl p =
  case winningLine cfg b pl p of
    Just line -> Won pl (FiveInRow line)
    Nothing
      | M.size b >= n * n -> Draw
      | otherwise         -> InProgress
  where n = unSize (cfgSize cfg)

-- | Đặt quân. Mọi lỗi trả về dưới dạng 'Left', không bao giờ ném ngoại lệ.
applyMove :: Pos -> GameState -> Either MoveError GameState
applyMove p gs
  | gsStatus gs /= InProgress          = Left GameOver
  | not (inBounds (cfgSize cfg) p)     = Left (OutOfBounds p)
  | M.member p (gsBoard gs)            = Left (Occupied p)
  | otherwise = Right gs
      { gsBoard   = b'
      , gsTurn    = other pl
      , gsHistory = Move pl p : gsHistory gs
      , gsStatus  = statusAfter cfg b' pl p
      , gsClock   = clockLimit cfg
      }
  where
    cfg = gsConfig gs
    pl  = gsTurn gs
    b'  = M.insert p pl (gsBoard gs)

-- | Hoàn tác nước đi gần nhất (và đưa ván về trạng thái đang chơi).
undoMove :: GameState -> Either MoveError GameState
undoMove gs = case gsHistory gs of
  []              -> Left NothingToUndo
  (Move pl p : r) -> Right gs
    { gsBoard   = M.delete p (gsBoard gs)
    , gsTurn    = pl
    , gsHistory = r
    , gsStatus  = InProgress
    , gsClock   = clockLimit (gsConfig gs)
    }

-- | Người chơi @loser@ đầu hàng. Không có tác dụng nếu ván đã kết thúc.
resign :: Player -> GameState -> GameState
resign loser gs
  | gsStatus gs == InProgress = gs { gsStatus = Won (other loser) Resignation }
  | otherwise                 = gs

-- | Trôi qua một giây. Hết giờ thì người đang đến lượt thua.
tick :: GameState -> GameState
tick gs
  | gsStatus gs /= InProgress = gs
  | otherwise = case cfgTime (gsConfig gs) of
      Unlimited -> gs
      PerMove _
        | c' <= 0   -> gs { gsClock = 0, gsStatus = Won (other (gsTurn gs)) Timeout }
        | otherwise -> gs { gsClock = c' }
  where c' = gsClock gs - 1

-- | Chơi lại một danh sách nước đi từ ván mới (dùng khi nạp tệp).
-- Nhờ 'Either', một tệp bị hỏng/gian lận bị phát hiện thay vì làm hỏng trạng thái.
replay :: Config -> [Pos] -> Either MoveError GameState
replay cfg = foldM (flip applyMove) (newGame cfg)

legalMoves :: GameState -> [Pos]
legalMoves gs
  | gsStatus gs /= InProgress = []
  | otherwise =
      [ Pos r c
      | r <- [0 .. n - 1], c <- [0 .. n - 1]
      , M.notMember (Pos r c) (gsBoard gs) ]
  where n = unSize (cfgSize (gsConfig gs))
