module Main where

import qualified Characters_test
import qualified Components.Plugboard_test
import qualified Components.Reflector_test
import Test.Hspec
import qualified Enigma_test

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  Characters_test.spec
  Components.Plugboard_test.spec
  Components.Reflector_test.spec
  Enigma_test.spec
