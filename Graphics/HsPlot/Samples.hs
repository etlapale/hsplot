{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.HsPlot.Samples (
  Cut, Colour, Clarity, Diamond(..), diamonds, dsmall,
  Car(..), mtcars,
  Economics(..), economics
)

where

import Control.Applicative

import Data.Time (Day, fromGregorian)
import Data.Vector (Vector)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Csv

import Graphics.HsPlot.Base (Scalable)
import Graphics.HsPlot.TH


loadCsv :: FromRecord a => FilePath -> IO (Vector a)
loadCsv p = do
  Right v <- decode True <$> BS.readFile p
  return v

data Cut = Fair | Good | VeryGood | Premium | Ideal
  deriving (Eq, Enum, Ord, Show, Bounded)
instance Scalable Cut

data Colour = D | E | F | G | H | I | J
  deriving (Eq, Enum, Ord, Show, Bounded)
instance Scalable Colour

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

instance FromField Day where
  parseField s = let (y1:y2:y3:y4:_:m1:m2:_:d1:d2:_) = BS8.unpack s
                 in return $ fromGregorian (read [y1,y2,y3,y4])
                                           (read [m1,m2]) (read [d1,d2])

data Economics = Economics { date :: Day        -- ^ Date
                           , pce :: Double      -- ^ Personal consumption expend
                           , population :: Int  -- ^ Total population
                           , psavert :: Double  -- ^ Personal savings rate
                           , uempmed :: Double  -- ^ Median unemploy duration
                           , unemploy :: Int    -- ^ Number of unemploymed
                           }
$(deriveFromRecordOffset ''Economics 1)

economics :: IO (Vector Economics)
economics = loadCsv "data/economics.csv"
