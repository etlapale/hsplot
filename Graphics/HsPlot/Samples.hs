{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.HsPlot.Samples (
  Cut, Colour, Clarity, Diamond(..), diamonds, dsmall,
  Car(..), mtcars
)

where

import Control.Applicative

import Data.Vector (Vector)
import qualified Data.ByteString.Lazy as BS
import Data.Csv

import Graphics.HsPlot.TH


loadCsv :: FromRecord a => FilePath -> IO (Vector a)
loadCsv p = do
  Right v <- decode True <$> BS.readFile p
  return v

data Cut = Fair | Good | VeryGood | Premium | Ideal
  deriving (Eq, Enum, Ord)
data Colour = D | E | F | G | H | I | J
  deriving (Eq, Enum, Ord)
data Clarity = I3 | I2 | I1 | SI2 | SI1 | VS2 | VS1 | VVS2 | VVS1 | IF | FL
  deriving (Eq, Enum, Ord, Show)
data Diamond = Diamond { carat :: Double
                       , cut :: Cut
                       , color :: Colour
                       , clarity :: Clarity
                       , depth :: Double
                       , table :: Double
                       , price :: Int
                       , x' :: Double
                       , y' :: Double
                       , z' :: Double
                       }
$(deriveFromRecordOffset ''Diamond 1)

diamonds :: IO (Vector Diamond)
diamonds = loadCsv "data/diamonds.csv"

dsmall :: IO (Vector Diamond)
dsmall = loadCsv "data/dsmall.csv"

data Car = Car { manufacturer :: String
               , model :: String
               , displ :: Double
               , year :: Int
               , cyl :: Int
               , trans :: String
               , drv :: String
               , city :: Int
               , hwy :: Int
               , fl :: Char
               , class' :: String
               }
$(deriveFromRecordOffset ''Car 1)

mtcars :: IO (Vector Car)
mtcars = loadCsv "data/mpg.csv"
