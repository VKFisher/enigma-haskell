module Enigma_test where

import Components.Historic
import qualified Components.Plugboard as PB
import Components.Rotor (Rotor)
import Control.Monad.Freer (Eff, Member)
import qualified Control.Monad.Freer.State as ST
import Enigma (Enigma (..), decrypt, runEnigma, substituteConventions)
import Error (EnigmaError)
import Test.Hspec

spec :: Spec
spec = describe "Enigma" $ do
  describe "enigma" $ do
    it "decrypts historic message #1" $ do
      let input :: Text
          input =
            "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA "
              <> "GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA "
              <> "TLPIF SVKDA SCTAC DPBOP VHJK"
      let expectedResult :: Text
          expectedResult =
            "AUFKL ABTEILUNG VON KURTINOWA KURTINOWA NORDWESTL SEBEZ SEBEZ UAFFLIEGERSTRASZERICHTUNG DUBROWKI "
              <> "DUBROWKI OPOTSCHKA OPOTSCHKA UM EINSACHTDREINULL UHRANGETRETEN ANGRIFF INF RGT "
      let testDecrypt :: Member EnigmaError effs => Eff effs Text
          testDecrypt = do
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
            let initialEnigmaState =
                  Enigma
                    { plugboard = pb,
                      rotor1 = r1,
                      rotor2 = r2,
                      rotor3 = r3,
                      reflector = historicReflectorB
                    }
            ST.evalState initialEnigmaState $ decrypt input

      let res = substituteConventions <$> runEnigma testDecrypt
      res `shouldBe` Right expectedResult
    it "decrypts historic message #2" $ do
      let input :: Text
          input =
            "GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC "
              <> "FHUHM UNZEF RDISI KBGPM YVXUZ"
      let expectedResult :: Text
          expectedResult =
            "FEINDLICHEINFANTERIEKOLONNEBEOBACHTET ANFANGSUEDAUSGANGBAERWALDE ENDEDREIKMOSTWAERTSNEUSTADT"
      let testDecrypt :: Member EnigmaError effs => Eff effs Text
          testDecrypt = do
            pb <-
              PB.plugboard
                [ ('A', 'M'),
                  ('F', 'I'),
                  ('N', 'V'),
                  ('P', 'S'),
                  ('T', 'U'),
                  ('W', 'Z')
                ]
            r1 <- historicRotorIII 'V' 'L'
            r2 <- historicRotorI 'M' 'B'
            r3 <- historicRotorII 'X' 'A'
            let initialEnigmaState =
                  Enigma
                    { plugboard = pb,
                      rotor1 = r1,
                      rotor2 = r2,
                      rotor3 = r3,
                      reflector = historicReflectorA
                    }
            ST.evalState initialEnigmaState $ decrypt input
      let res = substituteConventions <$> runEnigma testDecrypt
      res `shouldBe` Right expectedResult
    it "decrypts historic message #3" $ do
      let input :: Text
          input =
            "YKAE NZAP MSCH ZBFO CUVM RMDP YCOF HADZ IZME FXTH FLOL PZLF GGBO TGOX GRET DWTJ IQHL MXVJ WKZU ASTR"
      let expectedResult :: Text
          expectedResult =
            "STEUERE? TANAF? ORD? ANSTANDORTCHUAAACCCVIERNEUNNEUNZWOFAHRTZWONULSM  SCHARNHORSTHCO"
      let testDecrypt :: Member EnigmaError effs => Eff effs Text
          testDecrypt = do
            pb <-
              PB.plugboard
                [ ('A', 'N'),
                  ('E', 'Z'),
                  ('H', 'K'),
                  ('I', 'J'),
                  ('L', 'R'),
                  ('M', 'Q'),
                  ('O', 'T'),
                  ('P', 'V'),
                  ('S', 'W'),
                  ('U', 'X')
                ]
            r1 <- historicRotorVIII 'M' 'V'
            r2 <- historicRotorVI 'H' 'Z'
            r3 <- historicRotorIII 'A' 'U'
            let initialEnigmaState =
                  Enigma
                    { plugboard = pb,
                      rotor1 = r1,
                      rotor2 = r2,
                      rotor3 = r3,
                      reflector = historicReflectorB
                    }
            ST.evalState initialEnigmaState $ decrypt input

      let res = substituteConventions <$> runEnigma testDecrypt
      res `shouldBe` Right expectedResult
    it "correctly handles a double step situation" $ do
      let input :: Text
          input = "AAAAAA"
      let expectedFinalRotorStates :: Member EnigmaError effs => Eff effs (Rotor, Rotor, Rotor)
          expectedFinalRotorStates = do
            r1 <- historicRotorI 'A' 'U'
            r2 <- historicRotorII 'A' 'F'
            r3 <- historicRotorIII 'A' 'L'
            pure (r1, r2, r3)
      let finalRotorStates :: Member EnigmaError effs => Eff effs (Rotor, Rotor, Rotor)
          finalRotorStates = do
            pb <- PB.plugboard []
            r1 <- historicRotorI 'A' 'O'
            r2 <- historicRotorII 'A' 'D'
            r3 <- historicRotorIII 'A' 'K'
            let initialEnigmaState =
                  Enigma
                    { plugboard = pb,
                      rotor1 = r1,
                      rotor2 = r2,
                      rotor3 = r3,
                      reflector = historicReflectorB
                    }
            finalState <- ST.execState initialEnigmaState $ decrypt input
            pure (rotor1 finalState, rotor2 finalState, rotor3 finalState)
      runEnigma finalRotorStates `shouldBe` runEnigma expectedFinalRotorStates
