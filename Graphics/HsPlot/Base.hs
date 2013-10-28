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
  Aesthetics(..), Colour(..), Geometry(..), Layer(..), Plot(..), Scale(..), Shape(..),
  Factor(..), Scalable,
  aes, plot, pointLayer,
  defaultColourScale, colourGradientScale, colourHueScale,
)
where

import Control.Applicative
import Data.Function
import Data.Foldable (Foldable, forM_, minimum, maximum, toList)
import Data.Maybe
import Data.Ord (comparing)
import Data.Traversable
import Prelude hiding (minimum, maximum)

import Graphics.HsPlot.Colours
import Graphics.HsPlot.Scales


data Plot f a x y c p s h = Plot { points :: f a
                                 , aesthetics :: Aesthetics a x y c p s h
                                 , layers :: [Layer a x y c p s h]
                                 , scaleX :: Scale x Double
                                 , scaleY :: Scale y Double
                                 , scaleC :: Scale c Colour
                                 , scaleP :: Scale p Double
                                 --, scaleS :: Scale s Double
                                 , scaleH :: Scale h Shape
                                 }

data Layer a x y c p s h = Layer { geometry :: Geometry
                                 , laes :: Maybe (Aesthetics a x y c p s h)
                                 }

pointLayer :: Layer a x y c p s h
pointLayer = Layer { geometry = Point
                   , laes = Nothing
                   }
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
         Ord x, Enum x, Real x, Rangeable x,
         Ord y, Enum y, Real y, Rangeable y,
         Ord c, Enum c, Scalable c,
         Ord p, Enum p, Scalable p,
         Ord s, Real p,--, NiceNum s, Real s, Enum s
         Ord h, Enum h, Eq h, Bounded h, Scalable h
         )
     => f a                             -- ^ Dataset to plot
     -> Aesthetics a x y c p s h        -- ^ Default plot aesthetics
     -> [Layer a x y c p s h]           -- ^ Set of layers to display
     -> Plot f a x y c p s h            -- ^ Resulting scaled plot
plot p aes l = Plot { points = p
                    , aesthetics = aes
                    , layers = l
                    , scaleX = xscale
                    , scaleY = yscale
                    , scaleC = cscale
                    , scaleP = pscale
                    , scaleH = hscale
                    }
  where xscale = niceLinearRange xmin xmax
        yscale = reverseScaleMap $ niceLinearRange ymin ymax
        cscale = Scale cmin cmax [cmin..cmax] colourScheme
        pscale = linearRange 0 1 pmin pmax
        hscale = Scale hmin hmax [hmin..hmax] shapeScale
        --sscale = niceLinearRange smin smax
        xmin = mma aes p l minimum x
        xmax = mma aes p l maximum x
        ymin = mma aes p l minimum y
        ymax = mma aes p l maximum y
        cmin = mma aes p l minimum colour
        cmax = mma aes p l maximum colour
        smin = mma aes p l minimum size
        smax = mma aes p l maximum size
        pmin = mma aes p l minimum alpha
        pmax = mma aes p l maximum alpha
        hmin = mma aes p l minimum shape
        hmax = mma aes p l maximum shape
        --colourScheme :: c -> Colour
        colourScheme = defaultColourScale cmin cmax
        shapeScale x = toEnum $ on mod fromEnum x maxBound

-- |Find a specific element accross multiple layers.
-- A typical usage would be to find the minimum or the maximum.
mma :: (Applicative f, Traversable f, Ord n)
    => Aesthetics a x y c p s h
    -> f a
    -> [Layer a x y c p s h]
    -> (forall t j. (Traversable t, Ord j) => (t j -> j))
    -> (Aesthetics a x y c p s h -> a -> n)
    -> n
mma aes p l m i = m $ fmap (m . (<$> p) . i . fromMaybe aes . laes) l
