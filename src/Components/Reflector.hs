module Components.Reflector
  ( reflector,
    applyReflector,
    Reflector,
  )
where

import Characters (EnigmaChar, alphabetCharset, ensureCharsetValid, toEnigmaChar)
import Control.Monad.Freer
import qualified Data.Map.Strict as Map
import Error (EnigmaError, throwError)
import Fmt ((+|), (|+))

newtype Reflector = Reflector (Map EnigmaChar EnigmaChar) -- wiring map
  deriving (Show, Eq)

-- |
-- smart constructor
-- given a list of characters produces a reflector where each alphabet
-- character is mapped to a corresponding replacement bidirectionally
-- i.e., if A maps to B then B must also map to A
-- you can also think of it as a plugboard with 13 connected pairs
reflector :: Member EnigmaError effs => Text -> Eff effs Reflector
reflector chars = do
  enigmaChars <- traverse toEnigmaChar $ toString chars
  ensureCharsetValid enigmaChars
  Reflector <$> buildMap enigmaChars
  where
    buildMap enigmaChars = do
      let forwardMap = Map.fromList $ zip alphabetCharset enigmaChars
      let backwardMap = Map.fromList $ zip enigmaChars alphabetCharset
      when (forwardMap /= backwardMap) $ throwError "Reflector is not bidirectional"
      pure forwardMap

-- |
-- applies the given reflector to a character
applyReflector :: Reflector -> EnigmaChar -> EnigmaChar
applyReflector (Reflector charMap) char = case Map.lookup char charMap of
  Just ec -> ec
  -- we expect that the Nothing case never happens
  -- since we're reasonably confident in the validity of the inputs
  -- (i.e. that each character has a corresponding mapping)
  Nothing -> error $ "Could not find character '" +| char |+ "' in reflector map. This should never happen!"