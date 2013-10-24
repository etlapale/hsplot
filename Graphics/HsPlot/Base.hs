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

{-# LANGUAGE DefaultSignatures, FlexibleInstances, GeneralizedNewtypeDeriving, OverlappingInstances, RankNTypes, StandaloneDeriving #-}

module Graphics.HsPlot.Base (
  Aesthetics(..), Colour(..), Geometry(..), Layer(..), PlotPoint(..), Plot(..), Scale(..), Shape(..),
  Factor(..), Scalable,
  aes, plot,
  defaultColourScale, colourGradientScale, colourHueScale,
  plotPoints
)
where

import Control.Applicative
import Data.Function
import Data.Foldable (Foldable, forM_, minimum, maximum, toList)
import Data.Ord (comparing)
import Data.Traversable
import Prelude hiding (minimum, maximum)

import Graphics.HsPlot.Algorithms
import Graphics.HsPlot.Colours


data Plot f a x y c p s h = Plot { points :: f a
                                 , layers :: [Layer a x y c p s h]
                                 , scaleX :: Scale x Double
                                 , scaleY :: Scale y Double
                                 , scaleC :: Scale c Colour
                                 --, scaleS :: Scale s Double
                                 , scaleH :: Scale h Shape
                                 }

data Layer a x y c p s h = Layer { geometry :: Geometry
                                 , aesthetics :: Aesthetics a x y c p s h
                                 }

data Scale x t = Scale { scaleMin   :: x
                       , scaleMax   :: x
                       , scaleTicks :: [x]
                       , scaleMap   :: x -> t
                       }

-- |Convert a points in a Layer to a Traversable of PlotPoint's.
plotPoints :: (Traversable f)
           => Plot f a x y c p s h
           -> Aesthetics a x y c p s h
           -> [PlotPoint]
plotPoints p a = tZip4
  (fmap (scaleMap (scaleX p) . x a) $ points p)
  (fmap (scaleMap (scaleY p) . y a) $ points p)
  (fmap (scaleMap (scaleC p) . colour a) $ points p)
  (fmap (scaleMap (scaleH p) . shape a) $ points p)
 
--tZip3 :: Traversable f => f a -> f b -> f c -> [(a,b,c)]
--tZip3 m n p = zip3 (toList m) (toList n) (toList p)

tZip4 :: Traversable f => f a -> f b -> f c -> f d -> [(a,b,c,d)]
tZip4 m n o p = zip4 (toList m) (toList n) (toList o) (toList p)

zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _      _      _      _      = []

type PlotPoint = (Double, Double, Colour, Shape)

data Aesthetics a x y c p s h = Aesthetics { x :: a -> x
                                           , y :: a -> y
                                           , colour :: a -> c
                                           , alpha :: a -> p
                                           , size :: a -> s
                                           , shape :: a -> h
                                           }

aes :: Aesthetics a Double Double Double Double Double Int
aes = Aesthetics { x = const 0
                 , y = const 0
                 , colour = const 0
                 , alpha = const 1
                 , size = const 0
                 , shape = const 0
                 }

data Geometry = Point | Line

data Shape = Circle | Triangle | Square | Cross | DiagSquare
  deriving (Eq, Ord, Enum)

type Colour = (Double,Double,Double)

colourGradientScale :: (Ord a, Enum a)
                    => a -> a -> a -> Colour
colourGradientScale cmin cmax c = (0,0,realToFrac (fromEnum c - fromEnum cmin) / realToFrac (fromEnum cmax - fromEnum cmin))

colourHueScale :: (Ord a, Enum a)
               => a -> a -> a -> Colour
colourHueScale cmin cmax c = lch2rgb 65 100 $ 360 * realToFrac (fromEnum c - fromEnum cmin) / realToFrac (fromEnum cmax - fromEnum cmin + 1)
        
class (Enum a, Ord a) => Scalable a where
  defaultColourScale :: a -> a -> a -> Colour
  default defaultColourScale :: (Enum a, Ord a) => a -> a -> a -> Colour
  defaultColourScale = colourHueScale

--instance (Enum a, Ord a) => Enum a where
--  defaultColourScale = colourHueScale

instance Scalable Int where
  defaultColourScale = colourGradientScale

instance Scalable Double where
  defaultColourScale = colourGradientScale

data Factor a = Factor { fromFactor :: a }

deriving instance Show a => Show (Factor a)
deriving instance Eq a => Eq (Factor a)
deriving instance Ord a => Ord (Factor a)

instance Enum a => Enum (Factor a) where
  fromEnum = fromEnum . fromFactor
  toEnum = fromFactor . toEnum

--deriving instance Num a => Num (Factor a)


instance (Enum a, Ord a) => Scalable (Factor a) where
  defaultColourScale = colourHueScale

-- |Construct a Plot for the given dataset and layers.
plot :: (Applicative f, Traversable f,
         Ord x, NiceNum x, Real x, Enum x,
         Ord y, NiceNum y, Real y, Enum y,
         Ord c, Enum c, Scalable c,
         Ord s,--, NiceNum s, Real s, Enum s
         Ord h, Enum h, Eq h, Bounded h, Scalable h
         )
     => f a -> [Layer a x y c p s h] -> Plot f a x y c p s h
plot p l = Plot { points = p
                , layers = l
                , scaleX = xscale
                , scaleY = yscale
                , scaleC = cscale
                , scaleH = hscale
                }
  where xscale = niceLinearRange xmin xmax
        yscale = reverseScaleMap $ niceLinearRange ymin ymax
        cscale = Scale cmin cmax [cmin..cmax] colourScheme
        hscale = Scale hmin hmax [hmin..hmax] shapeScale
        --sscale = niceLinearRange smin smax
        xmin = mma p l minimum x
        xmax = mma p l maximum x
        ymin = mma p l minimum y
        ymax = mma p l maximum y
        cmin = mma p l minimum colour
        cmax = mma p l maximum colour
        smin = mma p l minimum size
        smax = mma p l maximum size
        hmin = mma p l minimum shape
        hmax = mma p l maximum shape
        --colourScheme :: c -> Colour
        colourScheme = defaultColourScale cmin cmax
        shapeScale x = toEnum $ on mod fromEnum x maxBound

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
