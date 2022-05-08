module Enigma_test where

import Characters (alphabetCharset, fromEnigmaChar)
import Components.Historic
import Components.Plugboard (Plugboard)
import qualified Components.Plugboard as PB
import Components.Reflector (Reflector)
import qualified Components.Reflector as RF
import Components.Rotor (Rotor)
import qualified Components.Rotor as RT
import Control.Monad.Freer (Eff, Member)
import qualified Control.Monad.Freer.State as ST
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet, unsafeFromSet)
import qualified Data.Text as T
import Enigma (Enigma (..), decrypt, runEnigma, substituteConventions)
import Error (EnigmaError)
import Relude.Extra (bimapF)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Gen, chooseInt, forAll, frequency, shuffle)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (elements)

messageTests :: Spec
messageTests = describe "messages" $ do
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

doubleStepTests :: Spec
doubleStepTests = describe "double step" $ do
  it "correctly updates state on double step" $ do
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

crashOnError :: Eff '[EnigmaError] a -> a
crashOnError x = case runEnigma x of
  Left err -> error err
  Right r -> r

unsafePairs :: [a] -> [(a, a)]
unsafePairs xs = f xs []
  where
    f [] acc = acc
    f [_] _ = error "cannot split odd number of chars into pairs"
    f (x1 : x2 : xs') acc = f xs' ((x1, x2) : acc)

instance Arbitrary Plugboard where
  arbitrary = do
    pairCount <- chooseInt (0, 10)
    pairChars <- shuffle alphabetCharset
    let pairs =
          pairChars
            & take (pairCount * 2)
            & unsafePairs
            & bimapF fromEnigmaChar fromEnigmaChar
    pure $ crashOnError $ PB.plugboard pairs

instance Arbitrary Reflector where
  arbitrary =
    let charsetGen :: Gen Text
        charsetGen = do
          chars <- shuffle alphabetCharset
          let pairs = unsafePairs $ fromEnigmaChar <$> chars
              pb1 = crashOnError $ PB.plugboard $ take PB.maxPairsInPlugboard pairs
              pb2 = crashOnError $ PB.plugboard $ drop PB.maxPairsInPlugboard pairs
          pure $ toText $ fromEnigmaChar . (PB.applyPlugboard pb2 . PB.applyPlugboard pb1) <$> alphabetCharset
     in crashOnError . RF.reflector <$> charsetGen

instance Arbitrary Rotor where
  arbitrary =
    let turnoverPositionsGen' :: (Int, Int) -> Gen (NESet Char)
        turnoverPositionsGen' bounds = do
          count <- chooseInt bounds
          positions <- fmap fromEnigmaChar . take count <$> shuffle alphabetCharset
          pure $ unsafeFromSet $ Set.fromList positions

        turnoverPositionGen :: Gen (NESet Char)
        turnoverPositionGen =
          frequency
            [ (15, turnoverPositionsGen' (1, 2)),
              (1, turnoverPositionsGen' (3, length alphabetCharset))
            ]
     in do
          charset <- toText . fmap fromEnigmaChar <$> shuffle alphabetCharset
          ringSetting <- elements $ fmap fromEnigmaChar alphabetCharset
          initialPosition <- elements $ fmap fromEnigmaChar alphabetCharset
          turnoverPositions <- turnoverPositionGen
          pure $
            crashOnError $
              RT.rotor
                charset
                turnoverPositions
                ringSetting
                initialPosition

instance Arbitrary Enigma where
  arbitrary = do
    plugboard <- arbitrary
    reflector <- arbitrary
    rotor1 <- arbitrary
    rotor2 <- arbitrary
    rotor3 <- arbitrary
    pure $
      Enigma
        { plugboard = plugboard,
          rotor1 = rotor1,
          rotor2 = rotor2,
          rotor3 = rotor3,
          reflector = reflector
        }

messageGen :: Gen Text
messageGen = do
  n <- chooseInt (0, 1000)
  s <- replicateM n charGen
  pure $ toText s
  where
    charGen :: Gen Char
    charGen = elements $ fmap fromEnigmaChar alphabetCharset

enigmaGen :: Gen (Text, Enigma)
enigmaGen = do
  msg <- messageGen
  enigmaState <- arbitrary
  pure (msg, enigmaState)

properies :: Spec
properies =
  describe "properties" $
    let runEnigma' :: (Text, Enigma) -> Text
        runEnigma' (input, initialEnigmaState) = crashOnError $ ST.evalState initialEnigmaState $ decrypt input
     in do
          prop "never encodes a letter as itself" $
            forAll enigmaGen $
              \(input, initialEnigmaState) ->
                do
                  let output = runEnigma' (input, initialEnigmaState)
                  traverse_ (uncurry shouldNotBe) $ T.zip input output

          prop "returns initial input after encrypt / decrypt rountrip" $
            forAll enigmaGen $
              \(input, initialEnigmaState) ->
                do
                  let output = runEnigma' (input, initialEnigmaState)
                  let roundtrip = runEnigma' (output, initialEnigmaState)
                  roundtrip `shouldBe` input

spec :: Spec
spec = describe "Enigma" $ do
  messageTests
  doubleStepTests
  properies
