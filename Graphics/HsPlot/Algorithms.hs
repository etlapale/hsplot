-- |
-- Module:      Graphics.HsPlot.Algorithms
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- Graphical plotting algorithms.

module Graphics.HsPlot.Algorithms (
  NiceNum, niceRange
)
where

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

-- |Return a nice range and tick spacing for a given interval.
niceRange :: (NiceNum a, Real a) => a -> a -> (a,a,a)
niceRange min max = (niceMin,niceMax,spacing)
  where niceMin = spacing * niceFloor (realToFrac min / realToFrac spacing)
        niceMax = spacing * niceCeiling (realToFrac max / realToFrac spacing)
        spacing = niceNum $ range `niceDiv` (maxTicks - 1) 
        range = niceInteger (max - min)
        maxTicks = 6
