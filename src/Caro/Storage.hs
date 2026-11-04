-- | Lưu và nạp ván cờ ra tệp.
--
-- Tệp chỉ ghi CẤU HÌNH + DANH SÁCH NƯỚC ĐI. Khi nạp, các nước được CHƠI LẠI
-- qua 'replay', nên một tệp sai luật bị từ chối thay vì tạo ra trạng thái sai.
module Caro.Storage
  ( encodeSave, decodeSave
  , saveGame, loadGame
  ) where

import Control.Concurrent.MVar
import Control.Exception (IOException, try)
import System.Directory (renameFile)

import Caro.Parser
import Caro.Pretty (Pretty (..))
import Caro.Rules (replay)
import Caro.Types

encodeSave :: GameState -> String
encodeSave gs = unlines
  [ "caro-save 1"
  , "size: "    ++ show (unSize (cfgSize cfg))
  , "variant: " ++ variantKey (cfgVariant cfg)
  , "time: "    ++ timeKey (cfgTime cfg)
  , "moves: "   ++ unwords (map posText (reverse (map mvPos (gsHistory gs))))
  ]
  where cfg = gsConfig gs

decodeSave :: String -> Either String GameState
decodeSave txt = case parseSave txt of
  Left e                  -> Left ("tệp lưu không hợp lệ (" ++ parseErrorSummary e ++ ")")
  Right (SaveData cfg ps) ->
    either (\e -> Left ("nước đi trong tệp sai luật: " ++ pretty e)) Right (replay cfg ps)

-- | Ghi an toàn: ghi ra tệp tạm rồi đổi tên, nên tệp cũ không bao giờ bị ghi dở.
-- 'MVar' đóng vai trò khóa (mutex) để luồng tự động lưu và lệnh @save@/@quit@
-- không ghi cùng lúc vào một tệp tạm.
saveGame :: MVar () -> FilePath -> GameState -> IO (Either String ())
saveGame lock path gs = withMVar lock $ \_ -> do
  r <- try $ do
    let tmp = path ++ ".tmp"
    writeFile tmp (encodeSave gs)
    renameFile tmp path
  pure (either (\e -> Left (show (e :: IOException))) Right r)

-- | 'readFile' đọc LƯỜI; ép đọc hết nội dung ngay (@length t `seq`@) để tệp được
-- đóng lại trước khi luồng tự động lưu ghi đè lên nó.
loadGame :: FilePath -> IO (Either String GameState)
loadGame path = do
  r <- try (readFile path >>= \t -> length t `seq` pure t)
  pure $ case r of
    Left e    -> Left (show (e :: IOException))
    Right txt -> decodeSave txt
