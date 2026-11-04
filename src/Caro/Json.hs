-- | Bộ mã hóa JSON tối giản (đủ cho giao diện đồ họa), không dùng thư viện ngoài.
module Caro.Json (J (..), render) where

import Data.Char (isControl, ord)
import Data.List (intersperse)
import Numeric (showHex)

data J
  = JNull
  | JBool Bool
  | JNum Int
  | JStr String
  | JArr [J]
  | JObj [(String, J)]
  deriving (Eq, Show)

render :: J -> String
render j = go j ""
  where
    go :: J -> ShowS
    go JNull      = showString "null"
    go (JBool b)  = showString (if b then "true" else "false")
    go (JNum n)   = shows n
    go (JStr s)   = str s
    go (JArr xs)  = showChar '[' . commaSep (map go xs) . showChar ']'
    go (JObj kvs) = showChar '{' . commaSep [ str k . showChar ':' . go v | (k, v) <- kvs ] . showChar '}'

    commaSep :: [ShowS] -> ShowS
    commaSep = foldr (.) id . intersperse (showChar ',')

    str :: String -> ShowS
    str s = showChar '"' . foldr ((.) . esc) id s . showChar '"'

    esc :: Char -> ShowS
    esc '"'  = showString "\\\""
    esc '\\' = showString "\\\\"
    esc '\n' = showString "\\n"
    esc '\r' = showString "\\r"
    esc '\t' = showString "\\t"
    esc c
      | isControl c = showString "\\u" . showString (pad4 (showHex (ord c) ""))
      | otherwise   = showChar c
    pad4 h = replicate (4 - length h) '0' ++ h
