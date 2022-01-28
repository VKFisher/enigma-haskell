module Components.Rotor
  ( Rotor,
    rotor,
    applyRotor,
    stepRotor,
    inTurnoverPosition,
    applyRotorForward,
    applyRotorBackward,
  )
where

import Characters (EnigmaChar, alphabetCharset, ensureCharsetValid, ensureUnique, toEnigmaChar)
import Control.Monad.Freer (Eff, Member)
import qualified Data.Map.Strict as Map
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Error (EnigmaError)
import Relude.Extra (bimapBoth)

data RotorWire = RotorWireForward Int | RotorWireBackward Int
  deriving (Eq, Ord)

data RotorType
  = RotorType
      (Map RotorWire Int) -- wiring map
      (NESet Int) -- turnover positions (at least one)

data Rotor = Rotor
  { rotorType :: RotorType,
    ringSetting :: Int,
    position :: Int
  }

indexedAlphabetCharset :: [(Int, EnigmaChar)]
indexedAlphabetCharset = zip [0 .. length alphabetCharset] alphabetCharset

enigmaCharToIndex :: EnigmaChar -> Int
enigmaCharToIndex c =
  let enigmaCharToIndexMap = Map.fromList $ (\(x, y) -> (y, x)) <$> indexedAlphabetCharset
   in case Map.lookup c enigmaCharToIndexMap of
        Just index -> index
        Nothing -> error "Could not find index for character"

indexToEnigmaChar :: Int -> EnigmaChar
indexToEnigmaChar i =
  let indexToEnigmaCharMap = Map.fromList indexedAlphabetCharset
   in case Map.lookup i indexToEnigmaCharMap of
        Just ec -> ec
        Nothing -> error "Could not find character for index"

rotor :: Member EnigmaError effs => Text -> NESet Char -> Char -> Char -> Eff effs Rotor
rotor chars turnoverPositions ringSetting initialPosition = do
  ringSetting' <- enigmaCharToIndex <$> toEnigmaChar ringSetting
  turnoverPositions' <- traverse toEnigmaChar $ NESet.toList turnoverPositions
  ensureUnique . toList $ turnoverPositions'
  initialPosition' <- enigmaCharToIndex <$> toEnigmaChar initialPosition
  enigmaChars <- traverse toEnigmaChar $ toString chars
  ensureCharsetValid enigmaChars
  pure $
    Rotor
      { rotorType =
          RotorType
            (buildMap enigmaChars)
            (NESet.fromList . fmap enigmaCharToIndex $ turnoverPositions'),
        ringSetting = ringSetting',
        position = initialPosition'
      }
  where
    buildMap enigmaChars = do
      let toIndexMap = Map.fromList . fmap (bimapBoth enigmaCharToIndex)
          forwardMap = Map.mapKeys RotorWireForward $ toIndexMap $ zip alphabetCharset enigmaChars
          backwardMap = Map.mapKeys RotorWireBackward $ toIndexMap $ zip enigmaChars alphabetCharset
       in Map.union forwardMap backwardMap

applyRotorForward :: Rotor -> EnigmaChar -> EnigmaChar
applyRotorForward r c = applyRotor r (RotorWireForward $ enigmaCharToIndex c)

applyRotorBackward :: Rotor -> EnigmaChar -> EnigmaChar
applyRotorBackward r c = applyRotor r (RotorWireBackward $ enigmaCharToIndex c)

applyRotor :: Rotor -> RotorWire -> EnigmaChar
applyRotor
  Rotor
    { rotorType = RotorType wiringMap _,
      ringSetting,
      position
    }
  rw =
    let applyOffset x = (x - ringSetting + position) `mod` length alphabetCharset
        reverseOffset x = (x + ringSetting - position) `mod` length alphabetCharset
        offsetRW = case rw of
          RotorWireForward x -> RotorWireForward $ applyOffset x
          RotorWireBackward x -> RotorWireBackward $ applyOffset x
        result = reverseOffset $ case Map.lookup offsetRW wiringMap of
          Just ec -> ec
          Nothing -> error "Could not find character in map"
     in indexToEnigmaChar result

stepRotor :: Rotor -> Rotor
stepRotor r@Rotor {position} = r {position = (position + 1) `rem` length alphabetCharset}

inTurnoverPosition :: Rotor -> Bool
inTurnoverPosition
  Rotor
    { rotorType = RotorType _ turnoverPositions,
      position
    } = position `NESet.member` turnoverPositions