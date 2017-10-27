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
    let initialState =
          SessionStatus{ ehlo     = False
                       , from     = Nothing
                       , to       = Nothing
                       , sock     = sock'
                       , sockaddr = addr
                       , handle   = h
                       }
    reply220 h
    evalStateT session initialState 
  mainLoop sock

session :: SessionM
session  = do
  s <- get
  (cmdText, params) <- lift $ (parseLine . T.pack) <$> hGetLine (handle s)
  let cmd = textToCommand cmdText
  case () of
    _
      | (cmd == EHLO || cmd == HELO) && (not $ ehlo s)-> doEHLO
      | cmd == QUIT -> doQUIT

doEHLO :: SessionM
doEHLO = do
  s <- get
  let SockAddrInet _ clientAddr = sockaddr s
  lift $ reply250toEHLO (handle s) (clientAddr)
  put $ s { ehlo = True }

doQUIT :: SessionM
doQUIT = do
  s <- get
  lift $ reply221 $ handle s
