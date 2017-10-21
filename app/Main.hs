module Main where
import Network.Socket
import Network.BSD
import System.IO
import Control.Monad.Fix (fix)
import Control.Concurrent
type Msg = String

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock  = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO()
runConn (sock, addr) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hSetNewlineMode hdl NewlineMode{ inputNL = CRLF,
                                   outputNL = CRLF}
  hostName <- getHostName
  hPutStrLn hdl $ "220 " ++ hostName ++ " hsmtpd"
  line <- hGetLine hdl
  return ()
