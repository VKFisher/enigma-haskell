module Components.Reflector_test where

import Characters (fromEnigmaChar, toEnigmaChar)
import Components.Reflector (applyReflector, reflector)
import Enigma (runEnigma)
import Test.Hspec

spec :: Spec
spec = describe "Reflector" $ do
  describe "reflector" $ do
    it "Constructs reflectors from valid inputs" $ do
      let res =
            runEnigma . traverse reflector $
              [ "EJMZALYXVBWFCRQUONTSPIKHGD",
                "yruhqsldpxngokmiebfzcwvjat",
                "FVPJIAOYEDRZXWGCTKUQSBNMHL",
                "ENKQAUYWJIcopblmdxZVFTHRGS",
                "RDOBJNTKVEHMLFCWZAXGYIPSUQ"
              ]
      isRight res `shouldBe` True
    it "Rejects invalid inputs" $ do
      let res =
            runEnigma . reflector
              <$> [ "", -- empty input
                    "abcdefghijklmnopqrsuvwxyz", -- not enough symbols
                    "UVWXYZABCDEFGHIJKLMNOPQRSTT", -- duplicate symbol
                    "UVWGHIJKLMNXYZABBDEFOPQRST", -- duplicate symbol with expected length
                    "UVWGHIJKLMNXYZA$CDEFOPQRST", -- invalid symbol
                    "RDOAJNTKVEHMLFCWZBXGYIPSUQ" -- not bidirectional
                  ]
      res
        `shouldBe` [ Left "Missing characters in charset: [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z]",
                     Left "Missing characters in charset: [T]",
                     Left "Not all characters are unique: UVWXYZABCDEFGHIJKLMNOPQRSTT",
                     Left "Not all characters are unique: UVWGHIJKLMNXYZABBDEFOPQRST",
                     Left "$ is not a valid character",
                     Left "Reflector is not bidirectional"
                   ]
  describe "applyReflector" $ do
    let applyReflector' pbInput char = do
          enigmaChar <- toEnigmaChar char
          pb <- reflector pbInput
          pure $ applyReflector pb enigmaChar
    it "Correctly applies reflector" $ do
      let reflectorInput = "EJMZALYXVBWFCRQUONTSPIKHGD"
      let res = fmap fromEnigmaChar . runEnigma . applyReflector' reflectorInput <$> ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String)
      res `shouldBe` (Right <$> "EJMZALYXVBWFCRQUONTSPIKHGD")

