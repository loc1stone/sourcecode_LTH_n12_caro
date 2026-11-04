-- | Tìm nước đi cho máy bằng NHIỀU LUỒNG chạy song song trên nhiều capability.
--
-- Thiết kế (fork-join):
--
--   1. chia các ô ứng viên cho @workers@ luồng theo kiểu chia bài (round-robin);
--   2. mỗi luồng tính 'Best' của phần mình và bỏ vào một 'MVar' riêng;
--   3. luồng gọi đợi lần lượt từng 'MVar' rồi gộp bằng '<>' (Monoid).
--
-- Không có trạng thái chung bị nhiều luồng cùng ghi, nên không có tranh chấp
-- dữ liệu; thứ tự xong việc của các luồng không ảnh hưởng kết quả vì phép gộp
-- giao hoán. 'MVar' đảm bảo mỗi kết quả được trao đúng một lần.
module Caro.Parallel (parBestMove, splitRoundRobin) where

import Control.Concurrent
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM)

import Caro.AI
import Caro.Types

splitRoundRobin :: Int -> [a] -> [[a]]
splitRoundRobin n xs = [ [ x | (i, x) <- zip [0 :: Int ..] xs, i `mod` n == k ] | k <- [0 .. n - 1] ]

parBestMove :: Int -> Int -> GameState -> IO (Maybe Pos)
parBestMove workers depth gs
  | gsStatus gs /= InProgress = pure Nothing
  | otherwise = do
      caps <- getNumCapabilities
      let chunks = filter (not . null) (splitRoundRobin (max 1 workers) (topLevel gs))
      boxes <- forM (zip [0 ..] chunks) $ \(i, chunk) -> do
        box <- newEmptyMVar
        -- forkOn: ghim luồng vào capability (i mod caps) để thật sự chạy song song.
        _ <- forkOn (i `mod` caps) $ do
          -- ĐÁNH GIÁ CHẶT: 'evaluate' ép tính xong 'Best' ngay TRONG luồng con.
          -- Nếu chỉ @putMVar box (foldMap ...)@, MVar sẽ chứa một thunk chưa tính
          -- và toàn bộ việc nặng bị dồn về luồng gọi — mất song song.
          r <- try (evaluate (foldMap (evalCandidate depth gs) chunk))
          putMVar box (r :: Either SomeException Best)
        pure box
      results <- mapM takeMVar boxes
      bests <- mapM (either throwIO pure) results
      pure (bestPos (mconcat bests))
