{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad

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
instance FromField Cut where
  parseField s | s == "Fair" = pure Fair
               | s == "Good" = pure Good
               | s == "Very Good" = pure VeryGood
               | s == "Premium" = pure Premium
               | s == "Ideal" = pure Ideal
               | otherwise = mzero

data Colour = D | E | F | G | H | I | J
instance FromField Colour where
  parseField s | s == "D" = pure D
               | s == "E" = pure E
               | s == "F" = pure F
               | s == "G" = pure G
               | s == "H" = pure H
               | s == "I" = pure I
               | s == "J" = pure J
               | otherwise = mzero

data Clarity = I3 | I2 | I1 | SI2 | SI1 | VS2 | VS1 | VVS2 | VVS1 | IF | FL
instance FromField Clarity where
  parseField s | s == "I3" = pure I3
               | s == "I2" = pure I2
               | s == "I1" = pure I1
               | s == "SI2" = pure SI2
               | s == "SI1" = pure SI1
               | s == "VS2" = pure VS2
               | s == "VS1" = pure VS1
               | s == "VVS2" = pure VVS2
               | s == "VVS1" = pure VVS1
               | s == "IF" = pure IF
               | s == "FL" = pure FL
               | otherwise = mzero

main = do
  Right v <- decode True <$> BS.readFile "diamonds.csv"
  renderToPNG "hsplot.png" $ plot (V.toList v) layers
  where layers = [Layer Point $ aes {x=carat, y=(realToFrac . price)}]
