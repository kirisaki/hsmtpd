{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
import Network.BSD
import System.IO
import System.Environment (getArgs)
import Control.Monad.Fix (fix)
import Control.Concurrent
import SMTPserver.Reply
import SMTPserver
import Data.Text

type Msg = String

main :: IO ()
main = do
  args  <- getArgs
  let port = (read $ args !! 0::PortNumber)
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet port iNADDR_ANY)
  listen s 2
  mainLoop s

mainLoop :: Socket -> IO()
mainLoop sock  = do
  connection <- accept sock
  forkIO (runSession connection)
  mainLoop sock

runSession :: (Socket, SockAddr) -> IO()
runSession (sock, addr) = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  hSetNewlineMode h NewlineMode{ inputNL = CRLF,
                                   outputNL = CRLF}
  reply220 h
  line <- hGetLine h
  let SockAddrInet _ hostAddr = addr
  let (cmd, param) = parseLine $ pack line
  let cmd' = toUpper cmd
  if param == "" then
    reply501 h
    else if cmd' == "EHLO" || cmd' == "HELO" then
           reply250toEHLO h hostAddr
    else
           reply503 h
    
  
  return ()
