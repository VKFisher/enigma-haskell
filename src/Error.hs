module Error where

import Control.Monad.Freer.Error
import qualified Control.Monad.Freer.Error as FE
import Control.Monad.Freer

type EnigmaError = Error Text

throwError :: Member EnigmaError effs => Text -> Eff effs a
throwError = FE.throwError