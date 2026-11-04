{-# LANGUAGE ScopedTypeVariables #-}
-- | Kết nối TCP giữa hai tiến trình (có thể ở hai máy khác nhau).
-- Chủ phòng 'hostGame' lắng nghe; người chơi còn lại 'joinGame' kết nối tới.
module Caro.Net (hostGame, joinGame) where

import Control.Exception (IOException, try)
import Network.Socket
import System.IO

-- | Lắng nghe ở cổng @port@, chặn cho tới khi có đúng một người kết nối.
hostGame :: Int -> IO Handle
hostGame port = withSocketsDo $ do
  lsock <- socket AF_INET Stream defaultProtocol
  setSocketOption lsock ReuseAddr 1
  bind lsock (SockAddrInet (fromIntegral port) 0)
  listen lsock 1
  (conn, _) <- accept lsock
  close lsock
  toHandle conn

-- | Kết nối tới @host:port@. Thử lần lượt các địa chỉ phân giải được
-- (ví dụ @localhost@ có thể ra cả IPv6 lẫn IPv4).
joinGame :: String -> Int -> IO Handle
joinGame host port = withSocketsDo $ do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
  tryAll addrs
  where
    tryAll [] = ioError (userError ("không kết nối được tới " ++ host ++ ":" ++ show port))
    tryAll (a : rest) = do
      r <- try $ do
        sock <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
        connect sock (addrAddress a)
        pure sock
      case r of
        Right sock -> toHandle sock
        Left (_ :: IOException) -> tryAll rest

toHandle :: Socket -> IO Handle
toHandle sock = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  hSetEncoding h utf8
  pure h
