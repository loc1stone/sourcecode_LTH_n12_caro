-- | Giao thức văn bản (mỗi dòng một gói tin) cho chế độ chơi qua mạng.
--
-- > HELLO 15 caro     -- chủ phòng báo cỡ bàn và luật cho người vào
-- > MOVE 7 7          -- nước đi (hàng, cột, tính từ 0)
-- > RESIGN
-- > BYE
module Caro.Protocol (Msg (..), renderMsg, parseMsg) where

import Text.Parsec

import Caro.Parser
import Caro.Types

data Msg
  = Hello Int Variant
  | MsgMove Pos
  | MsgResign
  | MsgBye
  deriving (Eq, Show)

renderMsg :: Msg -> String
renderMsg (Hello n v)         = "HELLO " ++ show n ++ " " ++ variantKey v
renderMsg (MsgMove (Pos r c)) = "MOVE " ++ show r ++ " " ++ show c
renderMsg MsgResign           = "RESIGN"
renderMsg MsgBye              = "BYE"

parseMsg :: String -> Either ParseError Msg
parseMsg = parse (blanks *> msg <* (eof <?> "hết gói tin")) "gói tin"
  where
    msg = choice
      [ Hello   <$> (keyword "hello" *> number) <*> variantP
      , MsgMove <$> (keyword "move" *> (Pos <$> number <*> number))
      , MsgResign <$ keyword "resign"
      , MsgBye    <$ keyword "bye"
      ] <?> "HELLO, MOVE, RESIGN hoặc BYE"
