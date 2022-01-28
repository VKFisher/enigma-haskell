module Enigma (runEnigma) where

-- import Characters (EnigmaChar)
-- import Components.Plugboard (Plugboard)
-- import Components.Reflector (Reflector)
-- import Components.Rotor (RotorState)
import Control.Monad.Freer (Eff, run)
import Control.Monad.Freer.Error (runError)
import Error (EnigmaError)

runEnigma :: Eff '[EnigmaError] a -> Either Text a
runEnigma = run . runError

-- data Enigma = Enigma
--   { plugboard :: Plugboard,
--     rotor3 :: RotorState,
--     rotor2 :: RotorState,
--     rotor1 :: RotorState,
--     reflector :: Reflector
--   }

-- encrypt :: Members '[EnigmaError, State Enigma] effs => Text -> Eff effs Text
-- encrypt = undefined

-- encrypt' :: Members '[EnigmaError, State Enigma] effs => EnigmaChar -> Eff effs EnigmaChar
-- encrypt' = undefined

