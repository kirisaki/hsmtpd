{-# LANGUAGE OverloadedStrings #-}
module SMTPserver
  ( module SMTPserver.Reply
  , parseLine
  ) where
import SMTPserver.Reply
import Data.Text as T


parseLine :: Text -> (Text, Text)
parseLine l = (cmd', param')
  where
    l' = strip l
    (cmd, param) = T.break (\c -> c == ' ') l'
    cmd' = strip cmd
    param' = strip param
