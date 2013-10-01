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
  niceNum, niceRange
)
where


-- |Return a nice number approximatively equal to x.
niceNum :: Bool -> Double -> Double
niceNum round x = niceFraction * (10 ^^ exponent)
  where exponent = floor $ logBase 10 x
        fraction = x / (10 ^^ exponent)
        niceFraction | round && fraction < 1.5 = 1
                     | round && fraction < 3 = 2
                     | round && fraction < 7 = 5
                     | round && fraction >= 7 = 10
                     | fraction <= 1 = 1
                     | fraction <= 2 = 2
                     | fraction <= 5 = 5
                     | otherwise = 10

-- |Return a nice range and tick spacing for a given interval.
niceRange :: Double -> Double -> (Double,Double,Double)
niceRange min max = (niceMin,niceMax,tickSpacing)
  where tickSpacing = niceNum True $ range / realToFrac (maxTicks - 1)
        range = niceNum False $ max - min
        niceMin = tickSpacing * realToFrac (floor (min / tickSpacing))
        niceMax = tickSpacing * realToFrac (ceiling (max / tickSpacing))
        maxTicks = 6
