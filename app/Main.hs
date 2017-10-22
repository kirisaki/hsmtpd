{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
import Network.BSD
import System.IO
import System.Environment (getArgs)
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Concurrent
import SMTPserver.Reply
import SMTPserver
import Data.Text as T

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
  (sock', addr) <- accept sock
  forkIO $ do
    return()
    h <- socketToHandle sock' ReadWriteMode
    hSetBuffering h NoBuffering
    hSetNewlineMode h NewlineMode{ inputNL = CRLF,
                                   outputNL = CRLF}
    evalStateT session (sock', addr, h, Start)
  mainLoop sock

testst :: StateT (Socket, SockAddr, Int) IO ()
testst = do
  (sock, addr, a) <- get
  lift $ print addr
  return ()

session :: StateT SessionStatus IO()
session  = do
  (sock, addr, h, state) <- get
  (cmdText, params) <- lift $ (parseLine . T.pack) <$> hGetLine h
  let cmd = textToCommand cmdText
  let SockAddrInet _ clientAddr = addr
  case state of
    Start -> if params == "" || cmd == None
             then lift $ reply501 h
             else if cmd == EHLO || cmd == HELO
                  then lift $ reply250toEHLO h clientAddr
                  else lift $ reply503 h
  session
