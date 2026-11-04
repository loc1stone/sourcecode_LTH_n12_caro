-- | Chế độ @--ascii@: đổi chữ tiếng Việt có dấu thành không dấu để đọc được trên
-- console không hiển thị UTF-8 (ví dụ cmd/PowerShell cũ trên Windows, nơi chữ
-- thành @Cß¥ caro bß║░ng@). Mọi ký tự ngoài ASCII còn lại đều được thay, nên đầu ra
-- chắc chắn là ASCII thuần.
module Caro.Ascii (unaccent) where

import Data.Char (isAscii, toUpper)
import qualified Data.Map.Strict as M

-- | Chữ cái gốc và các dạng có dấu (chữ thường) của nó.
groups :: [(Char, String)]
groups =
  [ ('a', "àáạảãâầấậẩẫăằắặẳẵ")
  , ('e', "èéẹẻẽêềếệểễ")
  , ('i', "ìíịỉĩ")
  , ('o', "òóọỏõôồốộổỗơờớợởỡ")
  , ('u', "ùúụủũưừứựửữ")
  , ('y', "ỳýỵỷỹ")
  , ('d', "đ")
  ]

letters :: M.Map Char Char
letters = M.fromList
  [ pair | (base, accented) <- groups, c <- accented
         , pair <- [(c, base), (toUpper c, toUpper base)] ]

-- | Dấu câu Unicode thường gặp -> dạng ASCII.
punctuation :: M.Map Char String
punctuation = M.fromList
  [ ('…', "..."), ('–', "-"), ('—', "-"), ('·', "-"), ('•', "*")
  , ('“', "\""), ('”', "\""), ('‘', "'"), ('’', "'"), ('→', "->"), ('←', "<-") ]

unaccent :: String -> String
unaccent = concatMap conv
  where
    conv c
      | isAscii c                       = [c]
      | Just b <- M.lookup c letters    = [b]
      | Just s <- M.lookup c punctuation = s
      | otherwise                       = "?"
