-- |
-- Module:      Graphics.HsPlot.Scales
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- Scales algorithms.

{-# LANGUAGE DefaultSignatures #-}

module Graphics.HsPlot.Algorithms (
  Rangeable(..)
)
where

import Data.Time

data Scale x t = Scale { scaleMin   :: x
                       , scaleMax   :: x
                       , scaleTicks :: [x]
                       , scaleMap   :: x -> t
                       }

class NiceNum a where
  niceDiv :: a -> a -> a
  niceNum :: a -> a
  niceFloor :: RealFrac f => f -> a
  niceCeiling :: RealFrac f => f -> a

instance NiceNum Double where
  niceDiv = (/)
  niceNum = niceFloating
  niceFloor = realToFrac . floor
  niceCeiling = realToFrac . ceiling

instance NiceNum Int where
  niceDiv = div
  niceNum = niceInteger
  niceFloor = floor
  niceCeiling = ceiling

niceFloating :: (Fractional a, Real a) => a -> a
niceFloating x = niceFrac * (10 ^^ expo)
  where expo = floor $ logBase 10 (realToFrac x)
        frac = (realToFrac x) / (10 ^^ expo)
        niceFrac | frac < 1.5 = 1
                 | frac < 3   = 2
                 | frac < 7   = 5
                 | otherwise  = 10

niceInteger :: Real a => a -> a
niceInteger x = niceFrac * (10 ^ expo)
  where expo = floor $ logBase 10 (realToFrac x)
        frac = (realToFrac x) / (10 ^^ expo)
        niceFrac | frac <= 1 = 1
                 | frac <= 2 = 2
                 | frac <= 5 = 5
                 | otherwise = 10

class Rangeable a where
  niceRange :: a -> a -> (a,a,a)
  default niceRange :: (NiceNum a, Real a) => a -> a -> (a,a,a)
  niceRange = niceRange'

instance Rangeable Double where

instance Rangeable Int where

-- |Return a nice range and tick spacing for a given interval.
niceRange' :: (NiceNum a, Real a) => a -> a -> (a,a,a)
niceRange' min max = (niceMin,niceMax,spacing)
  where niceMin = spacing * niceFloor (realToFrac min / realToFrac spacing)
        niceMax = spacing * niceCeiling (realToFrac max / realToFrac spacing)
        spacing = niceNum $ range `niceDiv` (maxTicks - 1) 
        range = niceInteger (max - min)
        maxTicks = 6

linearRange :: (Real x, Fractional t)
            => x -> x -> x -> x -> Scale x t
linearRange imin imax min max = Scale { scaleMin = min
                            , scaleMax = max
                            , scaleTicks = [min,max]
                            , scaleMap = if min == max && min >= imin && min <= imax
                                         then realToFrac
                                         else (/(realToFrac $ max-min)) . realToFrac . (subtract min)
                            }
        
niceLinearRange :: (Rangeable x, Real x, Enum x, Fractional t)
                => x -> x -> Scale x t
niceLinearRange min max = Scale { scaleMin = min
                                , scaleMax = max
                                , scaleTicks = ticks
                                , scaleMap = (/(realToFrac $ max-min)) . realToFrac . subtract min
                                }
  where (niceMin,niceMax,niceSpacing) = niceRange min max
        ticks = filter (>=min) $ takeWhile (<=max) [niceMin + niceSpacing * i | i <- [0..]]

reverseScaleMap :: Num t => Scale x t -> Scale x t
reverseScaleMap s = s { scaleMap = (1-) . scaleMap s }
