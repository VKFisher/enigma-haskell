module Components.Historic
  ( historicReflectorA,
    historicReflectorB,
    historicReflectorC,
    historicReflectorBThin,
    historicReflectorCThin,
  )
where

import Components.Reflector (Reflector, reflector)
import Enigma (runEnigma)

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
