-- |
-- Module:      Graphics.HsPlot
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- A grammatical plotting library for Haskell.
-- 
-- Largely inspired by Wickham’s ggplot2 for R and Wilkinson’s
-- Grammar of Graphics, HsPlot is a general purpose plotting library
-- for Haskell. The Cairo backend allows to integrate HsPlot into various
-- kind of GUI programs or generate standalone vector and bitmap images.

{-# LANGUAGE RankNTypes #-}

module Graphics.HsPlot (
  Aesthetics(..), Geometry(..), Layer(..), Plot(..),
  aes, plot,
  render, renderToPNG
)
where

import Control.Applicative
--import Control.Monad
import Data.Foldable (Foldable, forM_, minimum, maximum, toList)
import Data.Traversable
import Prelude hiding (minimum, maximum)
import System.IO.Unsafe

import Graphics.Rendering.Cairo

import Graphics.HsPlot.Algorithms
import Graphics.HsPlot.Colours


data Plot f a x y c s h = Plot { points :: f a
                               , layers :: [Layer a x y c s h]
                               , scaleX :: Scale x Double
                               , scaleY :: Scale y Double
                               , scaleC :: Scale c Colour
                               --, scaleS :: Scale s Double
                               --, scaleH :: Scale h Shape
                               }

data Layer a x y c s h = Layer { geometry :: Geometry
                               , aesthetics :: Aesthetics a x y c s h
                               }

data Scale x t = Scale { scaleMin   :: x
                       , scaleMax   :: x
                       , scaleTicks :: [x]
                       , scaleMap   :: x -> t
                       }

data Aesthetics a x y c s h = Aesthetics { x :: a -> x
                                         , y :: a -> y
                                         , colour :: a -> c
                                         , size :: a -> s
                                         , shape :: a -> h
                                         }

aes :: (Num x, Num y, Num c, Num s, Num h) => Aesthetics a x y c s h
aes = Aesthetics { x = const 0
                 , y = const 0
                 , colour = const 0
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
     => f a -> [Layer a x y c s h] -> Plot f a x y c s h
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
        colorScheme c = lch2rgb 65 100 $ 360 * realToFrac (fromEnum c - fromEnum cmin) / realToFrac (fromEnum cmax - fromEnum cmin)

-- |Find a specific element accross multiple layers.
-- A typical usage would be to find the minimum or the maximum.
mma :: (Applicative f, Traversable f, Ord n)
    => f a
    -> [Layer a x y c s h]
    -> (forall t j. (Traversable t, Ord j) => (t j -> j))
    -> (Aesthetics a x y c s h -> a -> n)
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


renderToPNG :: (Applicative f, Traversable f, Show x, Show y)
            => FilePath -> Plot f a x y c s h -> IO ()
renderToPNG f p =
  withImageSurface FormatARGB32 w h $ \s -> do
    renderWith s $ render w h p
    surfaceWriteToPNG s f
  where w = 600
        h = 400
    
render :: (Applicative f, Traversable f, Show x, Show y)
       => Int -> Int -> Plot f a x y c s h -> Render ()
render w h p = do
  drawBackground

  -- Draw the layers
  rectangle ofx 0 lw lh
  clip
  translate ofx 0
  forM_ (layers p) $ drawLayer lw lh p
  resetClip

  -- Axes
  moveTo 0.5 0.5
  relLineTo 0 lh
  relLineTo lw 0
  setSourceRGBA 0.3 0.3 0.3 1
  setLineWidth 1
  stroke

  -- Ticks
  setSourceRGBA 0.2 0.2 0.2 1
  forM_ (scaleTicks $ scaleX p) $ \x -> do
    moveTo (lw * (realToFrac ((scaleMap $ scaleX p) x) :: Double)) lh
    relLineTo 0 (-tickSize)
  forM_ (scaleTicks $ scaleY p) $ \y -> do
    moveTo 0 (lh * (scaleMap $ scaleY p) y)
    relLineTo tickSize 0
  setLineWidth 2
  stroke

  -- Tick labels
  setSourceRGBA 0.2 0.2 0.2 1
  selectFontFace "Sans" FontSlantNormal FontWeightNormal
  setFontSize 10
  ftExt <- fontExtents
  let ascent = fontExtentsAscent ftExt
  forM_ (scaleTicks $ scaleX p) $ \x -> do
    moveTo (lw * (realToFrac ((scaleMap $ scaleX p) x) :: Double)) (lh + ascent + xticksmargin)
    let tickLabel = show x
    tExt <- textExtents tickLabel
    relMoveTo (-textExtentsWidth tExt/2) 0
    showText tickLabel
  forM_ (scaleTicks $ scaleY p) $ \y -> do
    moveTo 0 (lh * (scaleMap $ scaleY p) y)
    let tickLabel = show y
    tExt <- textExtents tickLabel
    relMoveTo (-textExtentsWidth tExt - yticksmargin) (textExtentsHeight tExt / 2)
    showText tickLabel
  fill

  translate (-ofx) 0

  where ofx = 40
        ofy = 30
        lw = realToFrac w - ofx
        lh = realToFrac h - ofy
        tickSize = 5
        xticksmargin = tickSize/2    -- Vertical margin on top of tick labels
        yticksmargin = tickSize      -- Horizontal margin right of tick labels

drawBackground :: Render ()
drawBackground = do
  setSourceRGB 1 1 1
  paint

drawLayer :: (Traversable f, Applicative f)
          => Double -> Double -> Plot f a x y c s h -> Layer a x y c s h -> Render ()
drawLayer w h p (Layer Point a) =
  drawPoints $ tZip3 (fmap ((*w) . scaleMap (scaleX p) . x a) $ points p)
                     (fmap ((*h) . scaleMap (scaleY p) . y a) $ points p)
                     (fmap (scaleMap (scaleC p) . colour a) $ points p)
drawLayer _ _ _ _ = undefined -- (Layer Line p) = drawLines p

tZip3 :: Traversable f => f a -> f b -> f c -> [(a,b,c)]
tZip3 m n p = zip3 (toList m) (toList n) (toList p)

drawPoints :: Foldable f => f (Double,Double,Colour) -> Render ()
drawPoints pts =
  forM_ pts $ \(x,y,(r,g,b)) -> do
    arc x y 2 0 (2*pi)
    setSourceRGB r g b
    fill

{-drawLines :: Foldable f => f (Double,Double) -> Render ()
drawLines ((x,y):p) = do
  moveTo (10*x) (10*y)
  lines p
  setSourceRGB 0 0 1
  setLineWidth 1
  stroke
  where lines ((x,y):p) = lineTo (10*x) (10*y) >> lines p
        lines [] = return ()-}
