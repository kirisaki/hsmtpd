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

session :: SessionM
session  = do
  (sock, addr, h, state) <- get
  (cmdText, params) <- lift $ (parseLine . T.pack) <$> hGetLine h
  let cmd = textToCommand cmdText
  let SockAddrInet _ clientAddr = addr
  case (state, cmd, params) of
    (Start, EHLO, params) -> doEHLO
    (Start, HELO, params) -> doEHLO
    (_, QUIT, _) -> doQUIT
    (_, RSET, _) -> doRSET
    (_, None, _) -> doNone
  (sock, addr, h, state) <- get
  lift $ hPutStrLn h $ show state
  if state == End
    then return ()
    else session

doRSET :: SessionM
doRSET = do
  (sock, addr, h, state) <- get
  lift $ reply250 h
  if state /= Start
    then put (sock, addr, h, AfterEHLO)
    else lift $ return ()

doQUIT :: SessionM
doQUIT = do
  (sock, addr, h, state) <- get
  lift $ reply221 h
  put (sock, addr, h, End)

doNone :: SessionM
doNone = do
  (sock, addr, h, state) <- get
  lift $ reply500 h

doEHLO :: SessionM
doEHLO = do
  (sock, addr, h, state) <- get
  let SockAddrInet _ clientAddr = addr
  lift $ reply250toEHLO h clientAddr
  put (sock, addr, h, AfterEHLO)
