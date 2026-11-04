-- | LÕI ỨNG DỤNG THUẦN: một hàm chuyển trạng thái
--
-- > step :: Event -> Session -> (Session, [Effect])
--
-- nhận một sự kiện (người gõ lệnh, đồng hồ tích tắc, máy tính xong nước đi, gói
-- tin từ đối thủ...) và trả về trạng thái mới cùng DANH SÁCH HIỆU ỨNG cần làm
-- (in ra, gửi gói tin, lưu tệp, khởi động máy...). Không có IO ở đây: phần có
-- hiệu ứng nằm hết ở "Caro.App". Nhờ vậy mọi luồng xử lý đều kiểm thử được.
module Caro.Session
  ( Mode (..), Session (..), Event (..), Effect (..), ActionError (..)
  , newSession, startSession, step, localTurn, botThinking, helpText, modeText
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

import Caro.AI
import Caro.Parser
import Caro.Pretty
import Caro.Protocol
import Caro.Rules
import Caro.Types

-- | Ai điều khiển bên nào.
data Mode
  = PvP                          -- ^ hai người cùng một máy
  | VsBot !Player !Difficulty    -- ^ người cầm quân đã cho, máy cầm quân còn lại
  | Online !Player               -- ^ chơi qua mạng; quân của mình
  deriving (Eq, Show)

data Session = Session
  { sGame     :: !GameState
  , sMode     :: !Mode
  , sVersion  :: !Int            -- ^ tăng mỗi khi ván cờ đổi (nước đi, undo, nạp, ván mới)
  , sSavePath :: FilePath
  , sBotFor   :: !(Maybe Int)    -- ^ phiên bản ván mà máy đã được giao tính nước
  , sHint     :: !(Maybe Pos)    -- ^ nước gợi ý gần nhất (xóa khi ván đổi)
  , sDone     :: !Bool
  } deriving (Eq, Show)

data Event
  = EvLine String                          -- ^ người gõ một dòng
  | EvTick                                 -- ^ đồng hồ: trôi qua 1 giây
  | EvBotMove Int Pos                      -- ^ máy tính xong (kèm phiên bản ván lúc bắt đầu tính)
  | EvNet Msg                              -- ^ gói tin từ đối thủ
  | EvNetBad String                        -- ^ gói tin không đọc được
  | EvNetClosed                            -- ^ mất kết nối
  | EvLoaded FilePath (Either String GameState)
  | EvNote String                          -- ^ cảnh báo từ luồng nền (lỗi)
  | EvQuiet String                         -- ^ ghi chú lặng lẽ từ luồng nền (ví dụ: đã tự lưu)
  | EvEof                                  -- ^ hết đầu vào
  deriving (Show)

-- | 'Say' là thông tin thường, 'Warn' là lỗi/cảnh báo (nước đi sai, lệnh sai...),
-- 'Quiet' là ghi chú phụ. Giao diện dòng lệnh in cả ba như nhau; giao diện đồ họa
-- tô màu khác nhau.
data Effect
  = Say String
  | Warn String
  | Quiet String
  | ShowBoard GameState
  | Send Msg
  | StartBot Int Int GameState             -- ^ phiên bản, độ sâu, ván cờ
  | SaveTo FilePath GameState
  | LoadFrom FilePath
  | Stop
  deriving (Show)

-- | Lỗi ở mức thao tác (có kiểu), bao gồm cả lỗi nước đi.
data ActionError
  = Illegal MoveError
  | NotYourTurn
  | Unsupported String
  deriving (Eq, Show)

instance Pretty ActionError where
  pretty (Illegal e)     = pretty e
  pretty NotYourTurn     = "Chưa đến lượt của bạn."
  pretty (Unsupported c) = "Lệnh '" ++ c ++ "' không dùng được ở chế độ chơi hiện tại."

newSession :: Mode -> GameState -> FilePath -> Session
newSession m g path = Session
  { sGame = g, sMode = m, sVersion = 0, sSavePath = path, sBotFor = Nothing, sHint = Nothing, sDone = False }

modeText :: Mode -> String
modeText PvP            = "Hai người chơi trên cùng một máy"
modeText (VsBot h lvl)  = "Bạn cầm " ++ show h ++ ", máy cầm " ++ show (other h)
                          ++ " (mức " ++ levelName lvl ++ ")"
modeText (Online me)    = "Chơi qua mạng, bạn cầm " ++ show me

helpText :: String
helpText = unlines
  [ "Lệnh:"
  , "  h8 | H 8 | 8h | 8 8 | 8,8   đặt quân (cột chữ + hàng số, hoặc hàng + cột)"
  , "  move h8 | play h8            như trên"
  , "  undo                         hoàn tác (đấu với máy: lùi cả nước của máy)"
  , "  hint                         gợi ý một nước đi"
  , "  resign                       đầu hàng"
  , "  save [tệp] | load tệp        lưu / nạp ván cờ"
  , "  new [cỡ]                     ván mới (cỡ bàn " ++ show minSize ++ ".." ++ show maxSize ++ ")"
  , "  show | help | quit" ]

-- | Khởi động: hiện bàn cờ và (nếu đến lượt máy) giao việc cho máy.
startSession :: Session -> (Session, [Effect])
startSession s = settle s
  [ Say ("== Cờ caro | " ++ modeText (sMode s) ++ " ==")
  , Say ("Luật: " ++ pretty (cfgVariant (gsConfig (sGame s))) ++ ".") ]

-- ---------------------------------------------------------------------------

step :: Event -> Session -> (Session, [Effect])
step ev s
  | sDone s   = (s, [])
  | otherwise = case ev of
      EvLine txt          -> onLine txt s
      EvTick              -> onTick s
      EvBotMove v p       -> onBotMove v p s
      EvNet m             -> onNet m s
      EvNetBad e          -> (s, [Warn ("Gói tin lạ từ đối thủ: " ++ e)])
      EvNetClosed         -> finish [Warn "Mất kết nối với đối thủ."] s
      EvLoaded path r     -> onLoaded path r s
      EvNote t            -> (s, [Warn t])
      EvQuiet t           -> (s, [Quiet t])
      EvEof               -> finish [] s

-- | Kết thúc phiên: lưu (nếu đã có nước đi, tránh ghi đè tệp cũ bằng ván trống).
finish :: [Effect] -> Session -> (Session, [Effect])
finish pre s =
  ( s { sDone = True }
  , pre ++ [ SaveTo (sSavePath s) g | not (null (gsHistory g)) ] ++ [Say "Tạm biệt!", Stop] )
  where g = sGame s

-- | Ghi nhận ván mới và tăng phiên bản.
commit :: GameState -> Session -> Session
commit g s = s { sGame = g, sVersion = sVersion s + 1, sHint = Nothing }

-- | Sau mỗi thay đổi: hiện bàn cờ và, nếu đến lượt máy, giao việc cho máy.
settle :: Session -> [Effect] -> (Session, [Effect])
settle s fx = (s', fx ++ [ShowBoard (sGame s)] ++ botFx)
  where (s', botFx) = triggerBot s

triggerBot :: Session -> (Session, [Effect])
triggerBot s = case sMode s of
  VsBot h lvl
    | gsStatus g == InProgress
    , gsTurn g /= h
    , sBotFor s /= Just (sVersion s) ->
        ( s { sBotFor = Just (sVersion s) }
        , [Say "Máy đang suy nghĩ...", StartBot (sVersion s) (depthOf lvl) g] )
  _ -> (s, [])
  where g = sGame s

-- | Người ngồi máy này có được đi lúc này không?
localTurn :: Session -> Bool
localTurn s = case sMode s of
  PvP        -> True
  VsBot h _  -> gsTurn (sGame s) == h
  Online me  -> gsTurn (sGame s) == me

-- | Máy đã được giao việc cho đúng phiên bản ván hiện tại và chưa trả lời.
botThinking :: Session -> Bool
botThinking s = case sMode s of
  VsBot h _ -> gsStatus g == InProgress && gsTurn g /= h && sBotFor s == Just (sVersion s)
  _         -> False
  where g = sGame s

isOnline :: Session -> Bool
isOnline s = case sMode s of { Online _ -> True; _ -> False }

-- | Quân do "mình" cầm (người dùng ở máy này).
myPlayer :: Session -> Player
myPlayer s = case sMode s of
  PvP        -> gsTurn (sGame s)
  VsBot h _  -> h
  Online me  -> me

-- ---------------------------------------------------------------------------
-- Người gõ lệnh

onLine :: String -> Session -> (Session, [Effect])
onLine txt s = case parseCommand txt of
  Left e
    | all isSpace txt -> (s, [])
    | otherwise       -> (s, [Warn (renderParseError txt e)])
  Right c -> onCommand c s

onCommand :: Command -> Session -> (Session, [Effect])
onCommand cmd s = case cmd of
  CHelp     -> (s, [Say helpText])
  CShow     -> (s, [ShowBoard g])
  CQuit     -> finish [Send MsgBye | isOnline s] s
  CMove p   -> humanMove p s
  CUndo     -> undoCmd s
  CHint     -> hintCmd s
  CResign   -> resignCmd s
  CSave mp  -> (s, [SaveTo (fromMaybe (sSavePath s) mp) g])
  CLoad p
    | isOnline s -> refuse "load"
    | otherwise  -> (s, [LoadFrom p])
  CNew msize
    | isOnline s -> refuse "new"
    | otherwise  -> newCmd msize s
  where
    g = sGame s
    refuse c = (s, [Warn (pretty (Unsupported c))])

humanMove :: Pos -> Session -> (Session, [Effect])
humanMove p s
  | gsStatus g /= InProgress = (s, [Warn (pretty (Illegal GameOver))])
  | not (localTurn s)        = (s, [Warn (pretty NotYourTurn)])
  | otherwise = case applyMove p g of
      Left e   -> (s, [Warn (pretty (Illegal e))])
      Right g' -> settle (commit g' s) [Send (MsgMove p) | isOnline s]
  where g = sGame s

undoCmd :: Session -> (Session, [Effect])
undoCmd s = case sMode s of
  Online _   -> (s, [Warn (pretty (Unsupported "undo"))])
  PvP        -> doUndo (const True)
  VsBot h _  -> doUndo (\g -> gsTurn g == h)
  where
    doUndo stopWhen = case undoMove (sGame s) of
      Left e   -> (s, [Warn (pretty (Illegal e))])
      Right g1 -> settle (commit (popUntil stopWhen g1) s) [Say "Đã hoàn tác."]
    -- Đấu với máy: lùi tiếp cho đến khi đến lượt người (hoặc hết nước để lùi).
    popUntil stopWhen g
      | stopWhen g = g
      | otherwise  = either (const g) (popUntil stopWhen) (undoMove g)

hintCmd :: Session -> (Session, [Effect])
hintCmd s
  | gsStatus g /= InProgress = (s, [Warn (pretty (Illegal GameOver))])
  | not (localTurn s)        = (s, [Warn (pretty NotYourTurn)])
  | otherwise = case bestMove (depthOf Normal) g of
      Just p  -> (s { sHint = Just p }, [Say ("Gợi ý: " ++ pretty p)])
      Nothing -> (s, [Warn "Không có gợi ý."])
  where g = sGame s

resignCmd :: Session -> (Session, [Effect])
resignCmd s
  | gsStatus g /= InProgress = (s, [Warn (pretty (Illegal GameOver))])
  | otherwise = settle (commit (resign (myPlayer s) g) s) [Send MsgResign | isOnline s]
  where g = sGame s

newCmd :: Maybe Int -> Session -> (Session, [Effect])
newCmd msize s =
  case maybe (Right cfg) (\n -> mkConfig n (cfgVariant cfg) (cfgTime cfg)) msize of
    Left e   -> (s, [Warn (pretty e)])
    Right c  -> settle (commit (newGame c) s) [Say "Bắt đầu ván mới."]
  where cfg = gsConfig (sGame s)

onLoaded :: FilePath -> Either String GameState -> Session -> (Session, [Effect])
onLoaded path r s = case r of
  Left err -> (s, [Warn ("Không nạp được " ++ path ++ ": " ++ err)])
  Right g  -> settle (commit g s) [Say ("Đã nạp ván cờ từ " ++ path ++ ".")]

-- ---------------------------------------------------------------------------
-- Đồng hồ

onTick :: Session -> (Session, [Effect])
onTick s
  | not (clockRuns s) = (s, [])
  | gsStatus g' /= InProgress = settle (commit g' s) [Warn "Hết giờ!"]
  | otherwise = (s { sGame = g' }, [ Warn ("Còn " ++ show (gsClock g') ++ " giây cho " ++ show (gsTurn g'))
                                    | gsClock g' `elem` [10, 5, 3, 2, 1] ])
  where
    g' = tick (sGame s)

-- | Đồng hồ chạy khi: có giới hạn thời gian, ván đang chơi, và đang đến lượt NGƯỜI
-- (thời gian máy nghĩ không bị tính; chế độ mạng không dùng đồng hồ).
clockRuns :: Session -> Bool
clockRuns s =
  cfgTime (gsConfig g) /= Unlimited
    && gsStatus g == InProgress
    && case sMode s of
         PvP        -> True
         VsBot h _  -> gsTurn g == h
         Online _   -> False
  where g = sGame s

-- ---------------------------------------------------------------------------
-- Máy và đối thủ mạng

-- | Kết quả của máy chỉ được nhận nếu ván CHƯA đổi kể từ lúc giao việc.
-- (Người có thể đã undo / nạp ván / bắt đầu ván mới trong lúc máy đang nghĩ.)
onBotMove :: Int -> Pos -> Session -> (Session, [Effect])
onBotMove v p s
  | v /= sVersion s = (s, [])
  | otherwise = case sMode s of
      VsBot h _
        | gsTurn g /= h -> case applyMove p g of
            Left e   -> (s, [Warn ("Máy chọn nước sai: " ++ pretty (Illegal e))])
            Right g' -> settle (commit g' s) [Say ("Máy đi " ++ pretty p ++ ".")]
      _ -> (s, [])
  where g = sGame s

onNet :: Msg -> Session -> (Session, [Effect])
onNet m s = case (sMode s, m) of
  (Online me, MsgMove p)
    | gsStatus g == InProgress && gsTurn g == other me -> case applyMove p g of
        Right g' -> settle (commit g' s) [Say ("Đối thủ đi " ++ pretty p ++ ".")]
        Left e   -> (s, [Warn ("Đối thủ gửi nước không hợp lệ: " ++ pretty (Illegal e))])
    | otherwise -> (s, [Warn "Đối thủ đi sai lượt; bỏ qua."])
  (Online me, MsgResign) ->
    settle (commit (resign (other me) g) s) [Say "Đối thủ đã đầu hàng."]
  (Online _, MsgBye) -> finish [Say "Đối thủ đã rời ván."] s
  _ -> (s, [])
  where g = sGame s
