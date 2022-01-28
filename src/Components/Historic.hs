module Components.Historic
  ( historicReflectorA,
    historicReflectorB,
    historicReflectorC,
    historicReflectorBThin,
    historicReflectorCThin,
    historicRotorI,
    historicRotorII,
    historicRotorIII,
    historicRotorIV,
    historicRotorV,
    historicRotorVI,
    historicRotorVII,
    historicRotorVIII,
  )
where

import Components.Reflector (Reflector, reflector)
import Components.Rotor (Rotor, rotor)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Set.NonEmpty as NESet
import Enigma (runEnigma)
import Error (EnigmaError)

-- |
-- for constructing reflectors from input that we know to be valid
-- used only for historic reflectors, not exported
unsafeReflector :: Text -> Reflector
unsafeReflector chars = case runEnigma $ reflector chars of
  Right r -> r
  Left e -> error e

historicReflectorA :: Reflector
historicReflectorA = unsafeReflector "EJMZALYXVBWFCRQUONTSPIKHGD"

historicReflectorB :: Reflector
historicReflectorB = unsafeReflector "YRUHQSLDPXNGOKMIEBFZCWVJAT"

historicReflectorC :: Reflector
historicReflectorC = unsafeReflector "FVPJIAOYEDRZXWGCTKUQSBNMHL"

historicReflectorBThin :: Reflector
historicReflectorBThin = unsafeReflector "ENKQAUYWJICOPBLMDXZVFTHRGS"

historicReflectorCThin :: Reflector
historicReflectorCThin = unsafeReflector "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

historicRotorI :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorI = rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" (NESet.fromList $ 'Q' :| [])

historicRotorII :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorII = rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" (NESet.fromList $ 'E' :| [])

historicRotorIII :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorIII = rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" (NESet.fromList $ 'V' :| [])

historicRotorIV :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorIV = rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" (NESet.fromList $ 'J' :| [])

historicRotorV :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorV = rotor "VZBRGITYUPSDNHLXAWMJQOFECK" (NESet.fromList $ 'Z' :| [])

historicRotorVI :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorVI = rotor "JPGVOUMFYQBENHZRDKASXLICTW" (NESet.fromList $ 'Z' :| ['M'])

historicRotorVII :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorVII = rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" (NESet.fromList $ 'Z' :| ['M'])

historicRotorVIII :: Member EnigmaError effs => Char -> Char -> Eff effs Rotor
historicRotorVIII = rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" (NESet.fromList $ 'Z' :| ['M'])
