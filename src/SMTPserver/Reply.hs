{-# LANGUAGE OverloadedStrings #-}
module SMTPserver.Reply
  ( reply220
  , reply250
  , reply250toEHLO
  , reply501
  , reply503
  ) where
import Network.BSD
import Network.Socket
import System.IO

reply220 :: Handle -> IO()
reply220 h = do
  hPutStr h "220 " >> getHostName >>= hPutStr h
  hPutStrLn h " ESMTP hsmtpd, service ready."

reply250 :: Handle -> IO()
reply250 h = hPutStr h "250 " >> hPutStrLn h "OK"

reply250toEHLO :: Handle -> HostAddress -> IO()
reply250toEHLO h addr = do
  hPutStr h "250 " >> getHostName >>= hPutStr h
  entry <- getHostByAddr AF_INET addr
  let host = hostName entry
  hPutStr h " Hello, " >> hPutStrLn h host

reply501 :: Handle -> IO()
reply501 h = hPutStrLn h "501 Syntax error in parameters or arguments"

reply503 :: Handle -> IO()
reply503 h = hPutStrLn h "503 Bad sequence of commands"
