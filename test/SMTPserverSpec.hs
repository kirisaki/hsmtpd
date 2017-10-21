{-# LANGUAGE OverloadedStrings #-}
module SMTPserverSpec(spec)where

import Test.Hspec
import SMTPserver

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
