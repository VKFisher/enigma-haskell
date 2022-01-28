module Characters
  ( toEnigmaChar,
    fromEnigmaChar,
    ensureCharsetValid,
    ensureUnique,
    alphabetCharset,
    EnigmaChar,
  )
where

import Control.Monad.Freer
import Data.Char (toUpper)
import qualified Data.Set as Set
import Error (EnigmaError, throwError)
import Fmt (Buildable, (+|), (|+))

-- |
-- characters that can be processed by the enigma machine
newtype EnigmaChar = EnigmaChar Char
  deriving (Ord, Eq, Show, Buildable)

_validCharacters :: Text
_validCharacters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphabetCharset :: [EnigmaChar]
alphabetCharset = EnigmaChar <$> toString _validCharacters

toEnigmaChar :: Member EnigmaError effs => Char -> Eff effs EnigmaChar
toEnigmaChar char =
  let uppercaseChar = toUpper char
   in if uppercaseChar `elem` toString _validCharacters
        then pure $ EnigmaChar uppercaseChar
        else throwError $ "" +| char |+ " is not a valid character"

fromEnigmaChar :: EnigmaChar -> Char
fromEnigmaChar (EnigmaChar c) = c

ensureUnique :: Member EnigmaError effs => [EnigmaChar] -> Eff effs ()
ensureUnique xs =
  if ordNub xs == xs
    then pass
    else throwError $ "Not all characters are unique: " +| fromEnigmaChar <$> xs |+ ""

-- |
-- check that a set of enigma characters is a rearrangement of alphabet characters
ensureCharsetValid :: Member EnigmaError effs => [EnigmaChar] -> Eff effs ()
ensureCharsetValid enigmaChars = do
  ensureUnique enigmaChars
  let validSet = Set.fromList alphabetCharset
  let inputSet = Set.fromList enigmaChars
  let invalidChars = inputSet `Set.difference` validSet
  let missingChars = validSet `Set.difference` inputSet
  unless (Set.null invalidChars) $ throwError $ "Invalid characters in charset: " +| Set.toList invalidChars |+ ""
  unless (Set.null missingChars) $ throwError $ "Missing characters in charset: " +| Set.toList missingChars |+ ""
