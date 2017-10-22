{-# LANGUAGE OverloadedStrings #-}
module SMTPserverSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import SMTPserver
import Data.Text as T

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "legal expression" $ do
      parseLine "ELHO klaraworks.net" `shouldBe` ("ELHO", "klaraworks.net")
    it "too much spaces" $ do
      parseLine "  ELHO   klaraworks.net  " `shouldBe` ("ELHO", "klaraworks.net")
    it "including tabs" $ do
      parseLine "\tELHO\t klaraworks.net" `shouldBe` ("ELHO", "klaraworks.net")
    it "space at value field" $ do
      parseLine "ELHO klaraworks net" `shouldBe` ("ELHO", "klaraworks net")
    it "no value" $ do
      parseLine "ELHO" `shouldBe` ("ELHO", "")
  describe "textToCommand" $ do
    it "legal expression" $ do
      textToCommand "MAIL" `shouldBe` MAIL
    it "includes lowercase" $ do
      textToCommand "Quit" `shouldBe` QUIT
    it "between spaces" $ do
      textToCommand " \tRSET  " `shouldBe` RSET
    it "wrong command" $ do
      textToCommand "voom" `shouldBe` None
    it "illegal expression" $ do
      textToCommand "a b c" `shouldBe` None
