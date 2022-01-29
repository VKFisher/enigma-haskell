module Main where

import Components.Historic
import qualified Components.Plugboard as PB
import Control.Monad.Freer (Eff, Member)
import qualified Control.Monad.Freer.State as ST
import Enigma
import Error (EnigmaError)

main :: IO ()
main =
  let input :: Text
      input =
        "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA "
          <> "GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA "
          <> "TLPIF SVKDA SCTAC DPBOP VHJK"
      machineState :: Member EnigmaError effs => Eff effs Enigma
      machineState = do
        pb <-
          PB.plugboard
            [ ('A', 'V'),
              ('B', 'S'),
              ('C', 'G'),
              ('D', 'L'),
              ('F', 'U'),
              ('H', 'Z'),
              ('I', 'N'),
              ('K', 'M'),
              ('O', 'W'),
              ('R', 'X')
            ]
        r1 <- historicRotorV 'L' 'A'
        r2 <- historicRotorIV 'U' 'L'
        r3 <- historicRotorII 'B' 'B'
        pure $
          Enigma
            { plugboard = pb,
              rotor1 = r1,
              rotor2 = r2,
              rotor3 = r3,
              reflector = historicReflectorB
            }

      result = runEnigma $ do
        machineState' <- machineState
        ST.evalState machineState' $ decrypt input
   in case result of
     Right res -> do
       putStrLn $ "Input  : " <> toString input
       putStrLn $ "Output : " <> toString (substituteConventions res)
     Left err -> putStrLn $ "Decryption failed due to error: " <> toString err
