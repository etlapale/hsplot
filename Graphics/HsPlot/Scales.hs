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

module Graphics.HsPlot.Scales (
  Scale(..), Rangeable(..),
  linearRange, niceLinearRange, reverseScaleMap
)
where

import Data.Time
import System.IO.Unsafe

data Scale x t = Scale { scaleMin   :: x
                       , scaleMax   :: x
                       , scaleTicks :: [x]
                       , scaleMap   :: x -> t
                       , scaleFmt   :: x -> String
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

instance NiceNum Integer where
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

  niceScale :: a -> a -> Scale a Double
  default niceScale :: (NiceNum a, Real a, Enum a, Show a, Fractional t)
                    => a -> a -> Scale a t
  niceScale min max = Scale { scaleMin = min
                            , scaleMax = max
                            , scaleTicks = ticks
                            , scaleMap = normalise min max
                            , scaleFmt = show
                            }
    where (niceMin,niceMax,niceSpacing) = niceRange min max
          ticks = filter (>=min) $ takeWhile (<=max) [niceMin + niceSpacing * i | i <- [0..]]

normalise :: (Real a, Num a, Fractional t) => a -> a -> a -> t
normalise min max = (/(realToFrac $ max-min)) . realToFrac . subtract min


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

linearRange :: (Real x, Show x, Fractional t)
            => x -> x -> x -> x -> Scale x t
linearRange imin imax min max =
  Scale { scaleMin = min
        , scaleMax = max
        , scaleTicks = [min,max]
        , scaleMap = if min == max && min >= imin && min <= imax
                     then realToFrac
                     else (/(realToFrac $ max-min)) . realToFrac . (subtract min)
        , scaleFmt = show
        }
        
niceLinearRange :: (Rangeable x, Real x, Enum x, Show x, Fractional t)
                => x -> x -> Scale x t
niceLinearRange min max = Scale { scaleMin = min
                                , scaleMax = max
                                , scaleTicks = ticks
                                , scaleMap = normalise min max
                                , scaleFmt = show
                                }
  where (niceMin,niceMax,niceSpacing) = niceRange min max
        ticks = filter (>=min) $ takeWhile (<=max) [niceMin + niceSpacing * i | i <- [0..]]

reverseScaleMap :: Num t => Scale x t -> Scale x t
reverseScaleMap s = s { scaleMap = (1-) . scaleMap s }

instance Rangeable Day where
  niceRange = undefined
  niceScale = niceDayScale

data TimeGranularity = Days | Weeks | Months | Years
  deriving (Eq, Ord, Enum, Show)

niceDayScale :: Day -> Day -> Scale Day Double
niceDayScale min max = Scale { scaleMin = min
                             , scaleMax = max
                             , scaleTicks = ticks spacing
                             , scaleMap = normaliseDays min max
                             , scaleFmt = fmt spacing
                             }
  where d = max `diffDays` min
        spacing :: (Integer, TimeGranularity)
        spacing | d < 7     = (1, Days)
                | d < 42    = (1, Weeks)
                | d < 84    = (2, Weeks)
                | d < 180   = (1, Months)
                | d < 366   = (2, Months)
                | d < 548   = (3, Months)
                | d < 731   = (4, Months)
                | d < 1096  = (6, Months)
                | otherwise = (yearSpacing, Years)
        fmt :: (Integer, TimeGranularity) -> (Day -> String)
        fmt (_, Years) = (\(y,_,_) -> show y) . toGregorian
        ticks :: (Integer, TimeGranularity) -> [Day]
        ticks (_, Days) = [min..max]
        ticks (n, Years) = unsafePerformIO $ do
          putStrLn $ "Year min: " ++ show ymin
          putStrLn $ "Day scale: " ++ show yearSpacing ++ " years"
          let ts = filter (>=min) $ takeWhile (<=max)
                     [(i*yearSpacing) `addGregorianYearsClip` ymin | i <- [0..]]
          putStrLn $ "niceInt " ++ show spacing
          putStrLn $ "Tick (" ++ show n ++ ", Years) => " ++ show ts
          return ts
        yearSpacing = floor $ niceFloating $ realToFrac daySpacing / 365.25
        daySpacing = niceInteger $ niceInteger (dmax - dmin) `div` 5
        dmin = toModifiedJulianDay min
        dmax = toModifiedJulianDay max
        ymin = case toGregorian min of 
                 (y,_,_) -> fromGregorian (zmod y yearSpacing) 1 1
        zmod x n = n * (x `div` n)

normaliseDays :: Day -> Day -> Day -> Double
normaliseDays min max = (/(realToFrac $ diffDays max min)) . realToFrac . (`diffDays` min)

    where spacing = niceInteger $ numDays `div` (maxTicks - 1)
          numDays = diffDays max min
          maxTicks = 6
