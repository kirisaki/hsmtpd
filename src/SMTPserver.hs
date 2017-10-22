{-# LANGUAGE OverloadedStrings #-}
module SMTPserver
  ( module SMTPserver.Reply
  , Command(..)
  , SessionStatus(..)
  , SessionState(..)
  , parseLine
  , textToCommand
  ) where
import SMTPserver.Reply
import Data.Text as T
import Text.Read(readMaybe)
import System.IO
import Network.Socket

data Command = ATRN | AUTH | BDAT | DATA | EHLO |
               ETRN | EXPN | HELO | HELP | MAIL |
               NOOP | QUIT | RCPT | RSET | SAML |
               SEND | SOML | STARTTLS  | TURN |
               VRFY | None deriving(Eq, Ord, Show, Read, Enum)
data SessionState = Start deriving(Eq, Show)
type Parameter = Text
type SessionStatus = (
  Socket,
  SockAddr,
  Handle,
  SessionState
  )
textToCommand :: Text -> Command
textToCommand t = case readMaybe $ T.unpack $ T.toUpper t of
                    Just c -> c
                    Nothing -> None

parseLine :: Text -> (Text, Text)
parseLine l = (cmd', param')
  where
    l' = strip l
    (cmd, param) = T.break (\c -> c == ' ') l'
    cmd' = strip cmd
    param' = strip param
