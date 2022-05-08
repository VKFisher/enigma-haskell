module Characters_test where

import Characters (toEnigmaChar, fromEnigmaChar)
import Enigma (runEnigma)
import Test.Hspec

spec :: Spec
spec = describe "Characters" $ do
  describe "toEnigmaChar" $ do
    let validCharacters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String
    it "accepts uppercase versions of valid characters" $ do
      let res = fmap fromEnigmaChar <<$>> traverse runEnigma $ toEnigmaChar <$> validCharacters
      res `shouldBe` Right validCharacters
    it "accepts lowercase versions of valid characters" $ do
      let res = fmap fromEnigmaChar <<$>> traverse runEnigma $ toEnigmaChar <$> ("abcdefghijklmnopqrstuvwxyz" :: String)
      res `shouldBe` Right validCharacters
    it "rejects invalid characters" $ do
      let res = fmap runEnigma $ toEnigmaChar <$> "$#!.@12()"
      res
        `shouldBe` [ Left "$ is not a valid character",
                     Left "# is not a valid character",
                     Left "! is not a valid character",
                     Left ". is not a valid character",
                     Left "@ is not a valid character",
                     Left "1 is not a valid character",
                     Left "2 is not a valid character",
                     Left "( is not a valid character",
                     Left ") is not a valid character"
                   ]
