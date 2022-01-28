module Components.Plugboard_test where

import Characters (fromEnigmaChar, toEnigmaChar)
import Components.Plugboard (applyPlugboard, plugboard)
import Enigma (runEnigma)
import Test.Hspec

spec :: Spec
spec = describe "Plugboard" $ do
  describe "plugboard" $ do
    it "Constructs plugboards from valid inputs" $ do
      let res =
            runEnigma . traverse plugboard $
              [ [],
                [('e', 'b')],
                [('A', 'B'), ('C', 'D')],
                [('e', 'B'), ('F', 'd')]
              ]
      isRight res `shouldBe` True
    it "Rejects invalid inputs" $ do
      let res =
            runEnigma . plugboard
              <$> [ [ ('A', 'B'),
                      ('C', 'D'),
                      ('E', 'F'),
                      ('G', 'H'),
                      ('I', 'J'),
                      ('K', 'L'),
                      ('M', 'N'),
                      ('O', 'P'),
                      ('Q', 'R'),
                      ('S', 'T'),
                      ('U', 'V')
                    ], -- too many pairs
                    [('e', 'E')], -- duplicate symbols in pairs
                    [('A', 'B'), ('a', 'b')], -- duplicate pairs
                    [('A', 'B'), ('B', 'A')], -- duplicate pairs with reverse order
                    [('A', 'B'), ('$', 'X')] -- pairs with invalid characters
                  ]
      res
        `shouldBe` [ Left "Plugboard cannot have more than 10 pairs",
                     Left "Not all characters are unique: EE",
                     Left "Not all characters are unique: ABAB",
                     Left "Not all characters are unique: ABBA",
                     Left "$ is not a valid character"
                   ]
  describe "applyPlugboard" $ do
    let applyPlugboard' pbInput char = do
          enigmaChar <- toEnigmaChar char
          pb <- plugboard pbInput
          pure $ applyPlugboard pb enigmaChar
    it "Correctly applies plugboard" $ do
      let pbInput = [('A', 'B'), ('C', 'D')]
      let res = fmap fromEnigmaChar . runEnigma . applyPlugboard' pbInput <$> ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String)
      res `shouldBe` (Right <$> "BADCEFGHIJKLMNOPQRSTUVWXYZ")
    it "Correctly applies empty plugboard" $ do
      let pbInput = []
      let res = fmap fromEnigmaChar . runEnigma . applyPlugboard' pbInput <$> ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String)
      res `shouldBe` (Right <$> "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
