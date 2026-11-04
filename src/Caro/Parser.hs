-- | Parser combinator (thư viện Parsec) cho:
--
--   * lệnh người chơi gõ vào ('parseCommand');
--   * tệp lưu ván cờ ('parseSave').
--
-- Parser hỗ trợ nhiều dạng đầu vào cho cùng một ý (@h8@, @H 8@, @8h@, @8 8@,
-- @8,8@, @move h8@ ...) và báo lỗi có chỉ vị trí bằng tiếng Việt.
module Caro.Parser
  ( Parser
  , Command (..), parseCommand, renderParseError, parseErrorSummary
  , SaveData (..), parseSave
  , keyword, lexeme, blanks, number, position, variantP
  ) where

import Control.Monad (when)
import Data.Char (isAlpha, isAlphaNum, isAscii, isSpace, toLower)
import Data.List (dropWhileEnd, intercalate, nub)
import Text.Parsec
import Text.Parsec.Error (Message (..), errorMessages)
import Text.Parsec.String (Parser)

import Caro.Pretty (Pretty (..))
import Caro.Types

-- ---------------------------------------------------------------------------
-- Thành phần cơ bản

-- | Bỏ khoảng trắng. Nhãn rỗng để Parsec không chèn "white space" (tiếng Anh)
-- vào thông báo lỗi.
blanks :: Parser ()
blanks = skipMany (satisfy isSpace) <?> ""

lexeme :: Parser a -> Parser a
lexeme p = p <* blanks

-- | Từ khóa không phân biệt hoa/thường; phải đứng riêng (không dính chữ/số phía sau).
-- @w@ phải viết bằng chữ thường.
keyword :: String -> Parser ()
keyword w = lexeme . try $ do
  mapM_ (\c -> satisfy ((== c) . toLower)) w
  notFollowedBy (satisfy isAlphaNum)

rawNumber :: Parser Int
rawNumber = do
  ds <- many1 (digit <?> "chữ số")
  when (length ds > 4) $ parserFail "số quá lớn"
  pure (read ds)

number :: Parser Int
number = lexeme rawNumber

asciiLetter :: Parser Char
asciiLetter = satisfy (\c -> isAscii c && isAlpha c) <?> "chữ cái (tên cột)"

colIndex :: Char -> Int
colIndex c = fromEnum (toLower c) - fromEnum 'a'

-- | Tọa độ. Các dạng chấp nhận được (hàng/cột đều tính từ 1):
--
--   * cột chữ + hàng số: @h8@, @H8@, @h 8@
--   * hàng số + cột chữ: @8h@, @8 h@
--   * hàng và cột đều là số: @8 8@, @8,8@, @8, 8@
position :: Parser Pos
position = lexeme (try letterFirst <|> numberFirst) <?> "tọa độ (ví dụ h8 hoặc 8 8)"
  where
    letterFirst = do
      c <- asciiLetter
      skipMany (char ' ')
      r <- rawNumber
      pure (Pos (r - 1) (colIndex c))
    numberFirst = do
      r <- rawNumber
      c <- try (skipMany (char ' ') *> (colIndex <$> asciiLetter))
           <|> (skipMany1 (oneOf " ,") *> (subtract 1 <$> rawNumber))
      pure (Pos (r - 1) c)

variantP :: Parser Variant
variantP =
  (Freestyle <$ keyword "freestyle") <|> (VietCaro <$ keyword "caro")
    <?> "tên luật (caro hoặc freestyle)"

-- ---------------------------------------------------------------------------
-- Lệnh người chơi

data Command
  = CMove Pos
  | CUndo
  | CHint
  | CShow
  | CHelp
  | CResign
  | CQuit
  | CSave (Maybe FilePath)
  | CLoad FilePath
  | CNew (Maybe Int)
  deriving (Eq, Show)

command :: Parser Command
command =
  choice
    [ CUndo   <$ keyword "undo"
    , CHint   <$ keyword "hint"
    , CShow   <$ (keyword "show" <|> keyword "board")
    , CHelp   <$ (keyword "help" <|> lexeme (char '?' >> pure ()))
    , CResign <$ keyword "resign"
    , CQuit   <$ (keyword "quit" <|> keyword "exit")
    , CSave   <$> (keyword "save" *> optionMaybe path)
    , CLoad   <$> (keyword "load" *> path)
    , CNew    <$> (keyword "new" *> optionMaybe number)
    , CMove   <$> (optional (keyword "move" <|> keyword "play") *> position)
    ] <?> "lệnh (help, undo, hint, save, load, new, resign, quit) hoặc tọa độ như h8"
  where
    path = dropWhileEnd isSpace <$> many1 (noneOf "\r\n") <?> "tên tệp"

parseCommand :: String -> Either ParseError Command
parseCommand = parse (blanks *> command <* (eof <?> "hết lệnh")) "lệnh"

-- | Gom các lời giải thích của Parsec thành danh sách chuỗi.
expectations :: ParseError -> [String]
expectations e = nub [ m | Expect m <- errorMessages e, not (null m) ]

reasons :: ParseError -> [String]
reasons e = nub [ m | Message m <- errorMessages e, not (null m) ]

-- | Thông báo lỗi dễ hiểu: in lại dòng lệnh, chỉ vị trí lỗi bằng dấu @^@.
renderParseError :: String -> ParseError -> String
renderParseError src e = unlines $
  [ "Không hiểu lệnh:"
  , "  " ++ src
  , "  " ++ replicate (col - 1) ' ' ++ "^"
  , problem
  ] ++ [ "Mong đợi: " ++ intercalate " hoặc " (expectations e) ++ "."
       | not (null (expectations e)) ]
    ++ [ "Gõ 'help' để xem hướng dẫn." ]
  where
    col = sourceColumn (errorPos e)
    problem = case reasons e of
      (r : _) -> "Lỗi: " ++ r ++ "."
      []
        | col > length src -> "Câu lệnh còn thiếu phần cuối."
        | otherwise        -> "Ký tự không mong đợi: '" ++ [src !! (col - 1)] ++ "'."

-- | Dạng gọn cho tệp lưu / gói tin mạng.
parseErrorSummary :: ParseError -> String
parseErrorSummary e =
  "dòng " ++ show (sourceLine p) ++ ", cột " ++ show (sourceColumn p)
    ++ (if null detail then "" else ": " ++ intercalate "; " detail)
  where
    p = errorPos e
    detail = reasons e ++ [ "mong đợi " ++ intercalate " hoặc " (expectations e)
                          | not (null (expectations e)) ]

-- ---------------------------------------------------------------------------
-- Tệp lưu:
--
-- > caro-save 1
-- > size: 15
-- > variant: caro
-- > time: unlimited
-- > moves: h8 i9 h9

data SaveData = SaveData { sdConfig :: Config, sdMoves :: [Pos] }
  deriving (Eq, Show)

parseSave :: String -> Either ParseError SaveData
parseSave = parse saveFile "tệp lưu"

saveFile :: Parser SaveData
saveFile = do
  blanks
  keyword "caro-save"
  ver <- number
  when (ver /= 1) $ parserFail "phiên bản tệp lưu không được hỗ trợ"
  sz <- field "size" number
  v  <- field "variant" variantP
  t  <- field "time" timeP
  ps <- field "moves" (many position)
  eof <?> "hết tệp"
  cfg <- either (parserFail . pretty) pure (mkConfig sz v t)
  pure (SaveData cfg ps)
  where
    field name p = keyword name *> lexeme (char ':') *> p
    timeP = (Unlimited <$ keyword "unlimited") <|> (PerMove <$> number)
              <?> "thời gian (số giây hoặc unlimited)"
