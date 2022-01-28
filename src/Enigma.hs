module Enigma (runEnigma, encrypt, decrypt, Enigma(..)) where

import Characters (EnigmaChar, fromEnigmaChar, toEnigmaChar)
import Components.Plugboard (Plugboard, applyPlugboard)
import Components.Reflector (Reflector, applyReflector)
import Components.Rotor (Rotor, applyRotorBackward, applyRotorForward, inTurnoverPosition, stepRotor)
import Control.Monad.Freer (Eff, Member, Members, run)
import Control.Monad.Freer.Error (runError)
import qualified Control.Monad.Freer.State as ST
import Error (EnigmaError)
import qualified Data.Text as T

runEnigma :: Eff '[EnigmaError] a -> Either Text a
runEnigma = run . runError

data Enigma = Enigma
  { plugboard :: Plugboard,
    rotor1 :: Rotor,
    rotor2 :: Rotor,
    rotor3 :: Rotor,
    reflector :: Reflector
  }

type EnigmaState = ST.State Enigma

encrypt :: Members '[EnigmaError, EnigmaState] effs => Text -> Eff effs Text
encrypt inputText = do
  inputEnigmaChars <- traverse toEnigmaChar $ toString $ T.filter (/= ' ') inputText
  outputEnigmaChars <- traverse encryptCharacter inputEnigmaChars
  pure . toText . fmap fromEnigmaChar $ outputEnigmaChars

-- |
-- same as encrypt, included for readability
decrypt :: Members '[EnigmaError, EnigmaState] effs => Text -> Eff effs Text
decrypt = encrypt

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

stepRotors :: Member (ST.State Enigma) effs => Eff effs ()
stepRotors = do
  r1InTurnover <- inTurnoverPosition <$> ST.gets rotor1
  r2InTurnover <- inTurnoverPosition <$> ST.gets rotor2

  -- turn slow rotor only if middle in turnover position
  when r2InTurnover (ST.modify (\en@Enigma {rotor3} -> en {rotor3 = stepRotor rotor3}))
  -- turn middle rotor if fast in turnover position or if double step conditions apply
  when (r1InTurnover || r2InTurnover) (ST.modify (\en@Enigma {rotor2} -> en {rotor2 = stepRotor rotor2}))
  -- turn fast rotor unconditionally
  ST.modify (\en@Enigma {rotor1} -> en {rotor1 = stepRotor rotor1})
