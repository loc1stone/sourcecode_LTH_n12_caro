-- | Máy đánh cờ: hàm THUẦN, xác định (cùng đầu vào cho cùng kết quả).
--
-- Cách làm: chấm điểm mỗi ô ứng viên bằng mẫu dãy quân (hai/ba/bốn quân, còn
-- mấy đầu mở), rồi nhìn trước @depth@ nước kiểu negamax giới hạn độ rộng.
-- Việc đánh giá TỪNG ô ứng viên ở tầng trên cùng độc lập với nhau, nên có thể
-- chia cho nhiều luồng chạy song song ("Caro.Parallel").
module Caro.AI
  ( Difficulty (..), depthOf, levelName
  , Best (..), bestPos
  , winScore, patternScore, moveScore
  , candidates, rankedMoves
  , valueOf, topLevel, evalCandidate, bestMove
  ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Caro.Rules
import Caro.Types

data Difficulty = Easy | Normal | Hard
  deriving (Eq, Show, Enum, Bounded)

-- | Số nước nhìn trước.
depthOf :: Difficulty -> Int
depthOf Easy   = 1
depthOf Normal = 2
depthOf Hard   = 4

levelName :: Difficulty -> String
levelName Easy   = "dễ"
levelName Normal = "vừa"
levelName Hard   = "khó"

-- | Điểm kết quả tốt nhất của một ứng viên: 'Semigroup'/'Monoid' để GỘP kết quả
-- của nhiều luồng. Phép gộp "lấy điểm cao hơn, hòa điểm thì lấy ô nhỏ hơn" có
-- tính kết hợp và giao hoán, nên kết quả KHÔNG phụ thuộc thứ tự các luồng xong việc.
data Best = NoBest | Best !Int !Pos
  deriving (Eq, Show)

instance Semigroup Best where
  NoBest <> b = b
  a <> NoBest = a
  a@(Best s1 p1) <> b@(Best s2 p2)
    | s1 > s2   = a
    | s2 > s1   = b
    | p1 <= p2  = a
    | otherwise = b

instance Monoid Best where
  mempty = NoBest

bestPos :: Best -> Maybe Pos
bestPos NoBest     = Nothing
bestPos (Best _ p) = Just p

winScore :: Int
winScore = 1000000

-- | Điểm của một dãy: độ dài và số đầu còn mở (0, 1 hoặc 2).
patternScore :: Variant -> Int -> Int -> Int
patternScore v len opens
  | len >= winLength = if v == VietCaro && opens == 0 then 0 else winScore
  | otherwise = case (len, opens) of
      (4, 2) -> 50000
      (4, 1) -> 5000
      (3, 2) -> 5000
      (3, 1) -> 300
      (2, 2) -> 300
      (2, 1) -> 30
      (1, 2) -> 10
      (1, 1) -> 2
      _      -> 0

-- | Giá trị tức thời của việc @pl@ đặt quân ở @p@: tấn công + (4/5) phòng thủ
-- (tức là điểm nếu ĐỐI THỦ đặt vào đó) + chút thưởng cho ô gần tâm.
moveScore :: Config -> Board -> Player -> Pos -> Int
moveScore cfg b pl p = attack + (defense * 4) `div` 5 + centre
  where
    sz = cfgSize cfg
    v  = cfgVariant cfg
    score who = sum
      [ patternScore v (length (liStones li)) (opens li)
      | d <- directions
      , let li = lineInfo sz (M.insert p who b) who p d ]
    opens li = fromEnum (liOpenBack li) + fromEnum (liOpenFront li)
    attack  = score pl
    defense = score (other pl)
    n       = unSize sz
    mid     = n `div` 2
    centre  = max 0 (mid - max (abs (posRow p - mid)) (abs (posCol p - mid)))

-- | Ô ứng viên: các ô trống trong phạm vi 2 ô quanh quân đã đặt (bàn trống: ô giữa).
candidates :: Config -> Board -> [Pos]
candidates cfg b
  | M.null b  = [Pos mid mid]
  | otherwise = S.toList $ S.fromList
      [ q
      | Pos r c <- M.keys b
      , dr <- [-2 .. 2], dc <- [-2 .. 2]
      , let q = Pos (r + dr) (c + dc)
      , inBounds sz q, M.notMember q b ]
  where
    sz  = cfgSize cfg
    mid = unSize sz `div` 2

-- | @k@ ô ứng viên điểm cao nhất (hòa điểm: ô nhỏ hơn trước).
rankedMoves :: Int -> Config -> Board -> Player -> [(Int, Pos)]
rankedMoves k cfg b pl = take k (sortBy cmp scored)
  where
    scored = [ (moveScore cfg b pl p, p) | p <- candidates cfg b ]
    cmp (s1, p1) (s2, p2) = compare s2 s1 <> compare p1 p2

-- | Độ rộng tìm kiếm ở các tầng sâu và ở tầng trên cùng.
innerBreadth, topBreadth :: Int
innerBreadth = 8
topBreadth   = 12

-- | Giá trị của nước @p@ cho @pl@ khi nhìn trước @depth@ nước
-- (= lợi ích của mình - giá trị phản công tốt nhất của đối thủ).
valueOf :: Config -> Int -> Board -> Player -> Pos -> Int
valueOf cfg depth b pl p
  | gain >= winScore || depth <= 1 = gain
  | otherwise                      = gain - reply
  where
    gain = moveScore cfg b pl p
    b'   = M.insert p pl b
    opp  = other pl
    reply = case rankedMoves innerBreadth cfg b' opp of
      [] -> 0
      ms -> maximum [ valueOf cfg (depth - 1) b' opp q | (_, q) <- ms ]

-- | Các ô ở tầng trên cùng cần đánh giá — đây là phần được chia cho các luồng.
topLevel :: GameState -> [Pos]
topLevel gs = map snd (rankedMoves topBreadth (gsConfig gs) (gsBoard gs) (gsTurn gs))

evalCandidate :: Int -> GameState -> Pos -> Best
evalCandidate depth gs p =
  Best (valueOf (gsConfig gs) depth (gsBoard gs) (gsTurn gs) p) p

-- | Bản TUẦN TỰ. Bản song song ('Caro.Parallel.parBestMove') phải cho cùng kết quả.
bestMove :: Int -> GameState -> Maybe Pos
bestMove depth gs
  | gsStatus gs /= InProgress = Nothing
  | otherwise = bestPos (foldMap (evalCandidate depth gs) (topLevel gs))
