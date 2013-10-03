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

module Graphics.HsPlot (
  Aesthetics(..), Geometry(..), Layer(..), Plot(..),
  aes, plot,
  renderToPNG
)
where

import Control.Applicative
import Control.Monad
import System.IO.Unsafe

import Graphics.Rendering.Cairo

import Graphics.HsPlot.Algorithms


data Plot a = Plot { points :: [a]
                   , layers :: [Layer a]
                   , scaleX :: Scale
                   , scaleY :: Scale
                   }

data Layer a = Layer { geometry :: Geometry
                     , aesthetics :: Aesthetics a
                     }

data Scale = Scale { scaleMin   :: Double
                   , scaleMax   :: Double
                   , scaleTicks :: [Double]
                   , scaleMap   :: Double -> Double
                   }

data Aesthetics a = Aesthetics { x :: a -> Double
                               , y :: a -> Double
                               , colour :: a -> Colour
                               , size :: a -> Double
                               , shape :: a -> Shape
                               }

aes :: Aesthetics a
aes = Aesthetics { x = const 0
                 , y = const 0
                 , colour = const (0,0,0,1)
                 , size = const 2
                 , shape = const Circle
                 }

data Geometry = Point | Line

data Shape = Circle | Square

type Colour = (Double,Double,Double,Double)


-- |Construct a Plot for the given dataset and layers.
plot :: [a] -> [Layer a] -> Plot a
plot p l = Plot p l xscale yscale
  where xscale = niceLinearRange xmin xmax
        yscale = reverseScaleMap $ niceLinearRange ymin ymax
        xmin = mma minimum x
        ymin = mma minimum y
        xmax = mma maximum x
        ymax = mma maximum y
        mma m i = m $ m . (<$> p) . i . aesthetics <$> l

niceLinearRange :: Double -> Double -> Scale
niceLinearRange min max = Scale { scaleMin = min
                                , scaleMax = max
                                , scaleTicks = ticks
                                , scaleMap = (/(max-min)) . subtract min
                                }
  where (niceMin,niceMax,niceSpacing) = niceRange min max
        ticks = filter (>=min) $ takeWhile (<=max) [niceMin + niceSpacing * i | i <- [0..]]

reverseScaleMap :: Scale -> Scale
reverseScaleMap s = s { scaleMap = (1-) . scaleMap s }


renderToPNG :: FilePath -> Plot a -> IO ()
renderToPNG f p =
  withImageSurface FormatARGB32 w h $ \s -> do
    renderWith s $ render w h p
    surfaceWriteToPNG s f
  where w = 600
        h = 400
    
render :: Int -> Int -> Plot a -> Render ()
render w h p = do
  drawBackground

  -- Draw the layers
  translate ofx 0
  forM_ (layers p) $ drawLayer lw lh p

  -- Axes
  moveTo 0.5 0.5
  relLineTo 0 lh
  relLineTo lw 0
  setSourceRGB 0 0 0
  setLineWidth 1
  stroke

  -- Ticks
  forM_ (scaleTicks $ scaleX p) $ \x -> do
    moveTo (lw * (scaleMap $ scaleX p) x) lh
    relLineTo 0 (-tickSize)
  forM_ (scaleTicks $ scaleY p) $ \y -> do
    moveTo 0 (lh * (scaleMap $ scaleY p) y)
    relLineTo tickSize 0
  setLineWidth 2
  stroke
  translate (-ofx) 0

  where ofx = 20
        ofy = 20
        lw = realToFrac w - ofx
        lh = realToFrac h - ofy
        tickSize = 5

drawBackground :: Render ()
drawBackground = do
  setSourceRGB 1 1 1
  paint

drawLayer :: Double -> Double -> Plot a -> Layer a -> Render ()
drawLayer w h p (Layer Point a) = drawPoints $ zip3 (map ((*w) . scaleMap (scaleX p) . x a) $ points p) (map ((*h) . scaleMap (scaleY p) . y a) $ points p) (colour a <$> points p)
drawLayer _ _ _ _ = undefined -- (Layer Line p) = drawLines p

drawPoints :: [(Double,Double,Colour)] -> Render ()
drawPoints pts =
  forM_ pts $ \(x,y,(r,g,b,a)) -> do
    arc x y 2 0 (2*pi)
    setSourceRGBA r g b a
    fill

{-
drawLines :: [(Double,Double)] -> Render ()
drawLines ((x,y):p) = do
  moveTo (10*x) (10*y)
  lines p
  setSourceRGB 0 0 1
  setLineWidth 1
  stroke
  where lines ((x,y):p) = lineTo (10*x) (10*y) >> lines p
        lines [] = return ()
-}
