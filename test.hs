{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Data.List

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Data.Csv

import Graphics.HsPlot


data Diamond = Diamond { carat :: Double
                       , cut :: Cut
                       , colour' :: Colour
                       , clarity :: Clarity
                       , depth :: Double
                       , table :: Double
                       , price :: Int
                       , x' :: Double
                       , y' :: Double
                       , z' :: Double
                       }
instance FromRecord Diamond where
  parseRecord v | V.length v == 11 = Diamond <$> v.!1 <*> v.!2 <*> v.!3 <*> v.!4 <*> v.!5 <*> v.!6 <*> v.!7 <*> v.!8 <*> v.!9 <*> v.!10
                | otherwise = mzero

data Cut = Fair | Good | VeryGood | Premium | Ideal
  deriving Enum
instance FromField Cut where
  parseField = enumToField ["Fair", "Good", "Very Good", "Premium", "Ideal"]

data Colour = D | E | F | G | H | I | J
  deriving Enum
instance FromField Colour where
  parseField = enumToField ["D","E", "F", "G", "H", "I", "J"]

data Clarity = I3 | I2 | I1 | SI2 | SI1 | VS2 | VS1 | VVS2 | VVS1 | IF | FL
  deriving Enum
instance FromField Clarity where
  parseField = enumToField ["I3", "I2", "I1", "SI2", "SI1", "VS2", "VS1",
                            "VVS2", "VVS1", "IF", "FL"]

enumToField :: Enum a => [Field] -> Field -> Parser a
enumToField n s = case elemIndex s n of
                    Just x -> pure $ toEnum x
                    Nothing -> mzero


main = do
  Right v <- decode True <$> BS.readFile "diamonds.csv"
  renderToPNG "hsplot.png" $ plot (V.toList v) layers
  where layers = [Layer Point $ aes {x=carat, y=(realToFrac . price)}]
