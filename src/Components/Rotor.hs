module Components.Rotor where

import Characters (EnigmaChar)

data Rotor = Rotor

data RotorState = RotorState
  { rotor :: Rotor,
    ringSetting :: EnigmaChar,
    position :: EnigmaChar
  }