-- |
-- Module:      Graphics.HsPlot.Base
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- Base definitions for HsPlot.

{-# LANGUAGE RankNTypes #-}

module Graphics.HsPlot.Base (
  Aesthetics(..), Colour(..), Geometry(..), Layer(..), Plot(..), Scale(..),
  aes, plot
)
where

import Control.Applicative
import Data.Foldable (Foldable, forM_, minimum, maximum, toList)
import Data.Traversable
import Prelude hiding (minimum, maximum)

import Graphics.HsPlot.Algorithms
import Graphics.HsPlot.Colors

data Plot f a x y c p s h = Plot { points :: f a
                                 , layers :: [Layer a x y c p s h]
                                 , scaleX :: Scale x Double
                                 , scaleY :: Scale y Double
                                 , scaleC :: Scale c Colour
                                 --, scaleS :: Scale s Double
                                 --, scaleH :: Scale h Shape
                                 }

data Layer a x y c p s h = Layer { geometry :: Geometry
                                 , aesthetics :: Aesthetics a x y c p s h
                                 }

data Scale x t = Scale { scaleMin   :: x
                       , scaleMax   :: x
                       , scaleTicks :: [x]
                       , scaleMap   :: x -> t
                       }

data Aesthetics a x y c p s h = Aesthetics { x :: a -> x
                                           , y :: a -> y
                                           , colour :: a -> c
                                           , alpha :: a -> p
                                           , size :: a -> s
                                           , shape :: a -> h
                                           }

aes :: (Num x, Num y, Num c, Num p, Num s, Num h) => Aesthetics a x y c p s h
aes = Aesthetics { x = const 0
                 , y = const 0
                 , colour = const 0
                 , alpha = const 1
                 , size = const 0
                 , shape = const 0
                 }

data Geometry = Point | Line

data Shape = Circle | Square

type Colour = (Double,Double,Double)


-- |Construct a Plot for the given dataset and layers.
plot :: (Applicative f, Traversable f,
         Ord x, NiceNum x, Real x, Enum x,
         Ord y, NiceNum y, Real y, Enum y,
         Ord c, Enum c
         )
     => f a -> [Layer a x y c p s h] -> Plot f a x y c p s h
plot p l = Plot p l xscale yscale cscale
  where xscale = niceLinearRange xmin xmax
        yscale = reverseScaleMap $ niceLinearRange ymin ymax
        cscale = Scale cmin cmax [cmin..cmax] colorScheme
        xmin = mma p l minimum x
        xmax = mma p l maximum x
        ymin = mma p l minimum y
        ymax = mma p l maximum y
        cmin = mma p l minimum colour
        cmax = mma p l maximum colour
        --colorScheme c = (0,0,realToFrac (fromEnum c - fromEnum cmin) / realToFrac (fromEnum cmax - fromEnum cmin),1)
        colorScheme c = lch2rgb 65 100 $ 360 * realToFrac (fromEnum c - fromEnum cmin) / realToFrac (fromEnum cmax - fromEnum cmin + 1)

-- |Find a specific element accross multiple layers.
-- A typical usage would be to find the minimum or the maximum.
mma :: (Applicative f, Traversable f, Ord n)
    => f a
    -> [Layer a x y c p s h]
    -> (forall t j. (Traversable t, Ord j) => (t j -> j))
    -> (Aesthetics a x y c p s h -> a -> n)
    -> n
mma p l m i = m $ fmap (m . (<$> p) . i . aesthetics) l
        
niceLinearRange :: (NiceNum x, Real x, Enum x, Fractional t)
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
