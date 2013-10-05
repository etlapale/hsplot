{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Data.Csv

import Graphics.HsPlot
import Graphics.HsPlot.TH

data Cut = Fair | Good | VeryGood | Premium | Ideal
  deriving (Eq, Enum, Ord)
data Colour = D | E | F | G | H | I | J
  deriving (Eq, Enum, Ord)
data Clarity = I3 | I2 | I1 | SI2 | SI1 | VS2 | VS1 | VVS2 | VVS1 | IF | FL
  deriving (Eq, Enum, Ord, Show)
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
$(deriveFromRecordOffset ''Diamond 1)

main = do
  Right v <- decode True <$> BS.readFile "dsmall.csv"
  renderToPNG "hsplot.png" $ plot v layers
  where layers = [Layer Point $ aes {x=carat, y=price, colour=colour'}]
