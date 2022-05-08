module Enigma (runEnigma, encrypt, decrypt, substituteConventions, Enigma (..)) where

import Characters (EnigmaChar, fromEnigmaChar, toEnigmaChar)
import Components.Plugboard (Plugboard, applyPlugboard)
import Components.Reflector (Reflector, applyReflector)
import Components.Rotor (Rotor, applyRotorBackward, applyRotorForward, inTurnoverPosition, stepRotor)
import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (runError)
import qualified Control.Monad.Freer.State as ST
import qualified Data.Text as T
import Error (EnigmaError)

runEnigma :: Eff '[EnigmaError] a -> Either Text a
runEnigma = run . runError

data Enigma = Enigma
  { plugboard :: Plugboard,
    rotor1 :: Rotor,
    rotor2 :: Rotor,
    rotor3 :: Rotor,
    reflector :: Reflector
  }
  deriving stock (Show, Eq)

type EnigmaState = ST.State Enigma

-- |
-- encrypts a text given an initial machine state
encrypt :: Members '[EnigmaError, EnigmaState] effs => Text -> Eff effs Text
encrypt inputText = do
  inputEnigmaChars <- traverse toEnigmaChar $ toString $ T.filter (/= ' ') inputText
  outputEnigmaChars <- traverse encryptCharacter inputEnigmaChars
  pure . toText . fmap fromEnigmaChar $ outputEnigmaChars

-- |
-- same as encrypt, included for readability
decrypt :: Members '[EnigmaError, EnigmaState] effs => Text -> Eff effs Text
decrypt = encrypt

-- |
-- simulates the result of a single button press
--
-- steps the rotors, then produces the encrypted character
encryptCharacter :: Members '[EnigmaState] effs => EnigmaChar -> Eff effs EnigmaChar
encryptCharacter char = do
  stepRotors
  Enigma {plugboard, rotor1, rotor2, rotor3, reflector} <- ST.get
  pure $
    char
      & applyPlugboard plugboard
      & applyRotorForward rotor1
      & applyRotorForward rotor2
      & applyRotorForward rotor3
      & applyReflector reflector
      & applyRotorBackward rotor3
      & applyRotorBackward rotor2
      & applyRotorBackward rotor1
      & applyPlugboard plugboard

-- |
-- executes a single stepping iteration for the machine
stepRotors :: Member EnigmaState effs => Eff effs ()
stepRotors = do
  r1InTurnover <- ST.gets $ inTurnoverPosition . rotor1
  r2InTurnover <- ST.gets $ inTurnoverPosition . rotor2

  -- turn slow rotor only if middle in turnover position
  when r2InTurnover (ST.modify (\en@Enigma {rotor3} -> en {rotor3 = stepRotor rotor3}))
  -- turn middle rotor if fast in turnover position or if double step conditions apply
  when (r1InTurnover || r2InTurnover) (ST.modify (\en@Enigma {rotor2} -> en {rotor2 = stepRotor rotor2}))
  -- turn fast rotor unconditionally
  ST.modify (\en@Enigma {rotor1} -> en {rotor1 = stepRotor rotor1})

-- |
-- Substitutes historic abbreviations and conventions
--
-- "X" for a space,
-- "J" for a quotation mark,
-- "Q" for the two characters "CH"
substituteConventions :: Text -> Text
substituteConventions t =
  t
    & T.replace "X" " "
    & T.replace "J" "? "
    & T.replace "Q" "CH"