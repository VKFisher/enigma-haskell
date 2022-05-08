module Components.Plugboard
  ( Plugboard,
    plugboard,
    applyPlugboard,
    maxPairsInPlugboard,
  )
where

import Characters (EnigmaChar)
import qualified Characters
import Control.Monad.Freer
import qualified Data.Map.Strict as Map
import Error (EnigmaError, throwError)
import Fmt ((+|), (|+))
import Relude.Extra (traverseBoth)

newtype Plugboard = Plugboard (Map EnigmaChar EnigmaChar)
  deriving (Show, Eq)

maxPairsInPlugboard :: Int
maxPairsInPlugboard = 10

-- |
-- smart constructor
-- given a list of character pairs produces a plugboard where each pair is connected
plugboard :: Member EnigmaError effs => [(Char, Char)] -> Eff effs Plugboard
plugboard charPairs = do
  validCharPairs <- sequenceA $ traverseBoth Characters.toEnigmaChar <$> charPairs
  when (length charPairs > maxPairsInPlugboard) . throwError $
    "Plugboard cannot have more than " +| maxPairsInPlugboard |+ " pairs"
  Characters.ensureUnique $ concatMap (\(x, y) -> [x, y]) validCharPairs
  pure . Plugboard . Map.fromList . concatMap (\(x, y) -> [(x, y), (y, x)]) $ validCharPairs

-- |
-- applies the given plugboard to a character
-- characters in pairs get swapped to their counterpart
-- characters not in pairs are not changed
applyPlugboard :: Plugboard -> EnigmaChar -> EnigmaChar
applyPlugboard (Plugboard charMap) char = fromMaybe char (Map.lookup char charMap)
