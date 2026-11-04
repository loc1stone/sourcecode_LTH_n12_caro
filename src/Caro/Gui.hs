{-# LANGUAGE ScopedTypeVariables #-}
-- | Giao diện đồ họa chạy trên trình duyệt, do chính chương trình Haskell phục vụ.
--
-- Chương trình mở một máy chủ HTTP nhỏ (thư viện @network@, chỉ lắng nghe ở
-- 127.0.0.1) và phục vụ:
--
-- > GET  /                 trang giao diện (gui/index.html, nhúng lúc biên dịch)
-- > GET  /api/state?stamp= trạng thái ván cờ dạng JSON (long-poll bằng STM)
-- > POST /api/move?r=&c=   đặt quân       POST /api/undo | hint | resign | save | load | quit
-- > POST /api/new?size=    ván mới
--
-- Giao diện KHÔNG chứa luật chơi: mỗi thao tác được đổi thành đúng 'Event' như khi
-- người gõ lệnh ở dòng lệnh, đi qua cùng hàm thuần 'Caro.Session.step'. Vì vậy
-- nước đi sai / sai lượt / ván đã kết thúc... đều bị chặn và báo lỗi bằng đúng
-- các quy tắc đã kiểm thử.
--
-- Phần THUẦN (đọc yêu cầu HTTP, định tuyến, kiểm tra nguồn gốc, dựng JSON) tách
-- riêng khỏi phần IO (socket) để kiểm thử được.
module Caro.Gui
  ( -- * Nhật ký thông báo
    GuiMsg (..), MsgLog (..), emptyLog, appendMsg
    -- * Phần thuần
  , stateJson, stampOf
  , Request (..), parseRequestHead
  , Action (..), Route (..), route, actionEvent
    -- * Phần IO
  , Gui (..), guiOutput, bindGui, serveGui, openBrowser
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Info (os)
import System.Process (spawnProcess)
import Text.Read (readMaybe)

import Caro.App
import Caro.Gui.Assets (indexHtml)
import Caro.Json
import Caro.Pretty
import Caro.Rules (clockLimit)
import Caro.Session
import Caro.Types

-- ---------------------------------------------------------------------------
-- Nhật ký thông báo (hiển thị trong khung "Thông báo" của giao diện)

data GuiMsg = GuiMsg { gmId :: !Int, gmLevel :: !Severity, gmText :: String }
  deriving (Eq, Show)

-- | Mới nhất đứng đầu; chỉ giữ một số thông báo gần nhất.
data MsgLog = MsgLog { mlNext :: !Int, mlItems :: [GuiMsg] }
  deriving (Eq, Show)

emptyLog :: MsgLog
emptyLog = MsgLog 1 []

appendMsg :: Severity -> String -> MsgLog -> MsgLog
appendMsg sev txt (MsgLog n items) = MsgLog (n + 1) (take 40 (GuiMsg n sev txt : items))

-- ---------------------------------------------------------------------------
-- Trạng thái -> JSON (thuần)

levelName :: Severity -> String
levelName SevInfo  = "info"
levelName SevWarn  = "warning"
levelName SevQuiet = "quiet"

-- | "Dấu vân tay" của những gì giao diện nhìn thấy; đổi thì giao diện cần vẽ lại.
stampOf :: Session -> [GuiMsg] -> String
stampOf s msgs = intercalate "-"
  [ show (sVersion s)
  , show (case msgs of { (m : _) -> gmId m; [] -> 0 })
  , show (gsClock (sGame s))
  , show (botThinking s)
  , show (isJust (sHint s))
  ]

stateJson :: Session -> [GuiMsg] -> J
stateJson s msgs = JObj
  [ ("stamp",       JStr (stampOf s msgs))
  , ("version",     JNum (sVersion s))
  , ("size",        JNum (unSize (cfgSize cfg)))
  , ("variant",     JStr (variantKey (cfgVariant cfg)))
  , ("variantText", JStr (pretty (cfgVariant cfg)))
  , ("mode",        JStr modeKey)
  , ("modeText",    JStr (modeText (sMode s)))
  , ("me",          maybe JNull (JStr . show) me)
  , ("turn",        JStr (show (gsTurn g)))
  , ("moves",       JArr [ JArr [JNum r, JNum c, JStr (show pl)]
                         | Move pl (Pos r c) <- reverse (gsHistory g) ])
  , ("status",      statusJ (gsStatus g))
  , ("statusText",  JStr (pretty (gsStatus g)))
  , ("timed",       JBool (cfgTime cfg /= Unlimited))
  , ("clock",       JNum (gsClock g))
  , ("limit",       JNum (clockLimit cfg))
  , ("canMove",     JBool (gsStatus g == InProgress && localTurn s && not (sDone s)))
  , ("thinking",    JBool (botThinking s))
  , ("hint",        maybe JNull (\(Pos r c) -> JArr [JNum r, JNum c]) (sHint s))
  , ("messages",    JArr [ JObj [("id", JNum (gmId m)), ("level", JStr (levelName (gmLevel m))), ("text", JStr (gmText m))]
                         | m <- reverse msgs ])
  ]
  where
    g   = sGame s
    cfg = gsConfig g
    (modeKey, me) = case sMode s of
      PvP       -> ("pvp", Nothing)
      VsBot h _ -> ("bot", Just h)
      Online m  -> ("online", Just m)
    statusJ InProgress = JObj [("kind", JStr "progress")]
    statusJ Draw       = JObj [("kind", JStr "draw")]
    statusJ (Won w reason) = JObj $
      [ ("kind", JStr "won"), ("winner", JStr (show w)) ] ++ case reason of
        FiveInRow ps -> [ ("reason", JStr "five"), ("line", JArr [ JArr [JNum r, JNum c] | Pos r c <- ps ]) ]
        Timeout      -> [ ("reason", JStr "timeout") ]
        Resignation  -> [ ("reason", JStr "resign") ]

-- ---------------------------------------------------------------------------
-- Yêu cầu HTTP (thuần)

data Request = Request
  { rqMethod  :: String
  , rqPath    :: String
  , rqQuery   :: [(String, String)]
  , rqHeaders :: [(String, String)]   -- ^ tên đã viết thường
  } deriving (Eq, Show)

-- | Đọc phần đầu của yêu cầu HTTP (dòng yêu cầu + các header).
parseRequestHead :: String -> Maybe Request
parseRequestHead txt = case map (dropWhileEnd (== '\r')) (lines txt) of
  []           -> Nothing
  (reqLine : hs) -> case words reqLine of
    [method, target, ver] | "HTTP/" `isPrefixOf` ver ->
      let (path, q) = break (== '?') target
      in Just Request
           { rqMethod  = method
           , rqPath    = path
           , rqQuery   = parseQuery (drop 1 q)
           , rqHeaders = [ (map toLower (trim k), trim (drop 1 v))
                         | h <- takeWhile (not . null) hs
                         , let (k, v) = break (== ':') h, not (null v) ]
           }
    _ -> Nothing
  where trim = dropWhileEnd isSpace . dropWhile isSpace

parseQuery :: String -> [(String, String)]
parseQuery "" = []
parseQuery s  = [ (k, drop 1 v) | kv <- splitOn '&' s, let (k, v) = break (== '=') kv, not (null k) ]
  where
    splitOn c xs = case break (== c) xs of
      (a, [])       -> [a]
      (a, _ : rest) -> a : splitOn c rest

-- ---------------------------------------------------------------------------
-- Định tuyến (thuần)

data Action
  = AMove Pos | AUndo | AHint | AResign | ANew Int | ASave | ALoad | AQuit
  deriving (Eq, Show)

data Route
  = RIndex
  | RState String          -- ^ dấu vân tay giao diện đang có
  | RAct Action
  | RRefuse Int String     -- ^ mã trạng thái HTTP + lý do
  deriving (Eq, Show)

-- | Chỉ nhận yêu cầu từ chính trang này:
--
--   * @Host@ phải là localhost/127.0.0.1 đúng cổng (chống "DNS rebinding");
--   * nếu có @Origin@ thì phải trùng (chống trang web lạ gửi lệnh tới cổng của bạn).
--
-- Giao diện cũng chỉ có một số thao tác cố định — không có đường nào để trình duyệt
-- truyền tên tệp tùy ý cho @save@/@load@.
route :: Int -> Request -> Route
route port rq
  | lookup "host" (rqHeaders rq) `notElem` map Just allowedHosts = RRefuse 403 "Host không hợp lệ."
  | maybe False (`notElem` allowedOrigins) (lookup "origin" (rqHeaders rq)) = RRefuse 403 "Origin không hợp lệ."
  | otherwise = case (rqMethod rq, rqPath rq) of
      ("GET", "/")          -> RIndex
      ("GET", "/api/state") -> RState (fromMaybe "" (lookup "stamp" (rqQuery rq)))
      ("POST", "/api/move") ->
        case (intParam "r", intParam "c") of
          (Just r, Just c) | inRange r, inRange c -> RAct (AMove (Pos r c))
          _ -> RRefuse 400 "Tọa độ không hợp lệ."
      ("POST", "/api/new")  -> maybe (RRefuse 400 "Cỡ bàn không hợp lệ.") (RAct . ANew) (intParam "size")
      ("POST", "/api/undo")   -> RAct AUndo
      ("POST", "/api/hint")   -> RAct AHint
      ("POST", "/api/resign") -> RAct AResign
      ("POST", "/api/save")   -> RAct ASave
      ("POST", "/api/load")   -> RAct ALoad
      ("POST", "/api/quit")   -> RAct AQuit
      (m, p) | m `notElem` ["GET", "POST"] -> RRefuse 405 "Phương thức không được hỗ trợ."
             | otherwise                   -> RRefuse 404 ("Không tìm thấy " ++ p)
  where
    allowedHosts   = [ h ++ ":" ++ show port | h <- ["localhost", "127.0.0.1"] ]
    allowedOrigins = [ "http://" ++ h | h <- allowedHosts ]
    intParam k = lookup k (rqQuery rq) >>= readMaybe
    inRange n  = n >= 0 && n < maxSize

-- | Thao tác trên giao diện -> đúng sự kiện như khi người gõ lệnh.
actionEvent :: Session -> Action -> Event
actionEvent s a = EvLine $ case a of
  AMove p  -> posText p
  AUndo    -> "undo"
  AHint    -> "hint"
  AResign  -> "resign"
  ANew n   -> "new " ++ show n
  ASave    -> "save"
  ALoad    -> "load " ++ sSavePath s
  AQuit    -> "quit"

-- ---------------------------------------------------------------------------
-- Phần IO

data Gui = Gui
  { guiEnv  :: Env
  , guiLog  :: TVar MsgLog
  , guiPort :: Int
  }

-- | Đầu ra cho giao diện đồ họa: thông báo vào nhật ký (và in ra terminal);
-- không vẽ bàn cờ ASCII, không có dấu nhắc.
guiOutput :: TVar MsgLog -> (String -> IO ()) -> Output
guiOutput lg echo = Output
  { outLine   = \sev t -> atomically (modifyTVar' lg (appendMsg sev t)) >> echo t
  , outBoard  = \_ -> pure ()
  , outPrompt = pure ()
  }

-- | Mở cổng (chỉ trên 127.0.0.1). Cổng 0 = hệ điều hành chọn; trả về cổng thật.
bindGui :: Int -> IO (Socket, Int)
bindGui port = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 32
  name <- getSocketName sock
  pure (sock, case name of { SockAddrInet p _ -> fromIntegral p; _ -> port })

-- | Vòng nhận kết nối; mỗi kết nối chạy trong một luồng riêng.
serveGui :: Gui -> Socket -> IO ()
serveGui g sock = forever $ do
  (conn, _) <- accept sock
  void . forkIO $
    (handleConn g conn `catch` \(_ :: SomeException) -> pure ()) `finally` close conn

utf8 :: String -> B.ByteString
utf8 = TE.encodeUtf8 . T.pack

indexBytes :: B.ByteString
indexBytes = utf8 indexHtml

-- | Đọc cho đến hết phần đầu yêu cầu (dòng trống), tối đa 16 KB.
readHead :: Socket -> IO (Maybe String)
readHead sock = go B.empty
  where
    go acc
      | B.length acc > 16384 = pure Nothing
      | otherwise = do
          chunk <- recv sock 4096
          if B.null chunk then pure Nothing else do
            let acc' = acc <> chunk
                (hd, rest) = B.breakSubstring (BC.pack "\r\n\r\n") acc'
            if B.null rest then go acc' else pure (Just (BC.unpack hd))

respond :: Socket -> Int -> String -> B.ByteString -> IO ()
respond sock code ctype body = sendAll sock $ B.concat
  [ BC.pack (unlines' [ "HTTP/1.1 " ++ show code ++ " " ++ reason code
                      , "Content-Type: " ++ ctype
                      , "Content-Length: " ++ show (B.length body)
                      , "Cache-Control: no-store"
                      , "X-Content-Type-Options: nosniff"
                      , "Content-Security-Policy: default-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline'; img-src 'self' data:"
                      , "Connection: close"
                      , "" ])
  , body ]
  where
    unlines' = concatMap (++ "\r\n")
    reason c = case c of
      200 -> "OK"; 400 -> "Bad Request"; 403 -> "Forbidden"
      404 -> "Not Found"; 405 -> "Method Not Allowed"; _ -> "Error"

handleConn :: Gui -> Socket -> IO ()
handleConn g sock = do
  mh <- readHead sock
  case mh >>= parseRequestHead of
    Nothing -> respond sock 400 "text/plain; charset=utf-8" (utf8 "Yêu cầu không hợp lệ.")
    Just rq -> case route (guiPort g) rq of
      RIndex       -> respond sock 200 "text/html; charset=utf-8" indexBytes
      RState stamp -> waitState g stamp >>= respond sock 200 "application/json; charset=utf-8" . utf8
      RAct a       -> do
        s <- readTVarIO (envSession (guiEnv g))
        post (guiEnv g) (actionEvent s a)
        respond sock 200 "application/json; charset=utf-8" (utf8 "{\"ok\":true}")
      RRefuse code why -> respond sock code "text/plain; charset=utf-8" (utf8 why)

-- | LONG-POLL bằng STM: giữ yêu cầu cho đến khi trạng thái đổi so với dấu vân tay
-- của trình duyệt (hoặc hết 20 giây). 'check' tự 'retry' khi chưa có gì mới nên
-- không phải thăm dò liên tục; 'registerDelay' đặt hạn chót.
waitState :: Gui -> String -> IO String
waitState g clientStamp = do
  deadline <- registerDelay 20000000
  atomically $ do
    s    <- readTVar (envSession (guiEnv g))
    msgs <- mlItems <$> readTVar (guiLog g)
    late <- readTVar deadline
    check (stampOf s msgs /= clientStamp || late)
    pure (render (stateJson s msgs))

-- | Thử mở trình duyệt mặc định (lỗi thì bỏ qua; người dùng vẫn mở được bằng địa chỉ in ra).
openBrowser :: String -> IO ()
openBrowser url = do
  let (cmd, args) = case os of
        "darwin"  -> ("open", [url])
        "mingw32" -> ("cmd", ["/c", "start", "", url])
        _         -> ("xdg-open", [url])
  r <- try (spawnProcess cmd args)
  case r of
    Left (_ :: SomeException) -> pure ()
    Right _                   -> pure ()
