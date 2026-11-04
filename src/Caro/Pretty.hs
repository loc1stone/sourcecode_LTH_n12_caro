-- | Lớp kiểu 'Pretty': cách hiển thị thân thiện với người dùng cho nhiều kiểu
-- khác nhau (quân cờ, tọa độ, lỗi, trạng thái, cả bàn cờ).
-- Khác với 'Show' (dành cho gỡ lỗi), 'Pretty' là văn bản tiếng Việt cho người chơi.
module Caro.Pretty (Pretty (..)) where

import qualified Data.Map.Strict as M

import Caro.Types

class Pretty a where
  pretty :: a -> String

  -- | Phương thức có cài đặt mặc định: ghép nhiều giá trị, cách nhau bởi dấu cách.
  prettyList :: [a] -> String
  prettyList = unwords . map pretty

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty Player where
  pretty = show

instance Pretty Pos where
  pretty = posText

instance Pretty Move where
  pretty (Move pl p) = pretty pl ++ " " ++ pretty p

instance Pretty Variant where
  pretty Freestyle = "tự do (5 quân liên tiếp là thắng)"
  pretty VietCaro  = "caro (5 quân liên tiếp, không bị chặn cả hai đầu)"

instance Pretty MoveError where
  pretty GameOver         = "Ván cờ đã kết thúc."
  pretty (OutOfBounds p)  = "Ô " ++ pretty p ++ " nằm ngoài bàn cờ."
  pretty (Occupied p)     = "Ô " ++ pretty p ++ " đã có quân."
  pretty NothingToUndo    = "Không còn nước nào để hoàn tác."

instance Pretty ConfigError where
  pretty (BadSize n) =
    "Cỡ bàn phải từ " ++ show minSize ++ " đến " ++ show maxSize ++ " (nhận " ++ show n ++ ")."
  pretty (BadTime n) =
    "Giới hạn thời gian mỗi nước phải từ 5 đến 600 giây (nhận " ++ show n ++ ")."

instance Pretty Status where
  pretty InProgress = "Đang chơi."
  pretty Draw       = "Hòa: bàn cờ đã đầy."
  pretty (Won pl (FiveInRow line)) =
    show pl ++ " thắng với " ++ show (length line) ++ " quân liên tiếp!"
  pretty (Won pl Timeout)     = show pl ++ " thắng vì đối thủ hết giờ."
  pretty (Won pl Resignation) = show pl ++ " thắng vì đối thủ đầu hàng."

-- | Vẽ cả bàn cờ. @[X]@ là nước vừa đi, @<X>@ là dãy quân thắng.
instance Pretty GameState where
  pretty gs = unlines $
    [""] ++ [colRow] ++ map line [0 .. n - 1] ++ [colRow] ++ footer
    where
      cfg   = gsConfig gs
      n     = unSize (cfgSize cfg)
      lastP = case gsHistory gs of
                (Move _ p : _) -> Just p
                []             -> Nothing
      wins  = case gsStatus gs of
                Won _ (FiveInRow ps) -> ps
                _                    -> []
      colRow = "   " ++ concat [ [' ', colLetter c, ' '] | c <- [0 .. n - 1] ]
      line r = pad2 (show (r + 1)) ++ " " ++ concatMap (cell r) [0 .. n - 1]
               ++ " " ++ show (r + 1)
      pad2 s = replicate (2 - length s) ' ' ++ s
      cell r c =
        let p = Pos r c
        in case M.lookup p (gsBoard gs) of
             Nothing -> " . "
             Just pl
               | p `elem` wins   -> '<' : show pl ++ ">"
               | Just p == lastP -> '[' : show pl ++ "]"
               | otherwise       -> ' ' : show pl ++ " "
      footer = case gsStatus gs of
        InProgress ->
          [ "Nước thứ " ++ show (length (gsHistory gs) + 1)
              ++ " | Lượt: " ++ show (gsTurn gs)
              ++ clockText ]
        st -> [pretty st]
      clockText = case cfgTime cfg of
        Unlimited -> ""
        PerMove _ -> " | Còn lại: " ++ show (gsClock gs) ++ "s"
