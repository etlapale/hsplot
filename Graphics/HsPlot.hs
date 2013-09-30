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
  Aesthetics(..), Layer(..), Geometry(..),
  plot, aes,
  renderToPNG
)
where

import Control.Applicative
import Control.Monad
import Graphics.Rendering.Cairo

data Plot a = Plot { points :: [a]
                   , layers :: [Layer a]
                   , scaleX :: Scale
                   , scaleY :: Scale
                   }

data Layer a = Layer { geometry :: Geometry
                     , aesthetics :: Aesthetics a
                     }

type Scale = Double -> Double

data Aesthetics a = Aesthetics { x :: a -> Double
                               , y :: a -> Double
                               , colour :: a -> Colour
                               , size :: a -> Double
                               , shape :: a -> Shape
                               }

aes :: Aesthetics a
aes = Aesthetics { x = const 0
                 , y = const 0
                 , colour = const (0,0,0)
                 , size = const 2
                 , shape = const Circle
                 }

data Geometry = Point | Line

data Shape = Circle | Square

type Colour = (Double,Double,Double)

plot :: [a] -> [Layer a] -> Plot a
plot p l = Plot p l xscale yscale
  where xscale = (/(xmax-xmin)) . subtract xmin
        yscale = (/(ymax-ymin)) . subtract ymin
        xmin = mma minimum x
        ymin = mma minimum y
        xmax = mma maximum x
        ymax = mma maximum y
        mma m i = m $ m . (<$> p) . i . aesthetics <$> l

renderToPNG :: FilePath -> Plot a -> IO ()
renderToPNG f p =
  withImageSurface FormatARGB32 width height $ \s -> do
    renderWith s $ do
      drawBackground
      forM_ (layers p) $ drawLayer (realToFrac width) (realToFrac height) p
    surfaceWriteToPNG s f
  where width = 600
        height = 400

drawBackground :: Render ()
drawBackground = do
  setSourceRGB 1 1 1
  paint

drawLayer :: Double -> Double -> Plot a -> Layer a -> Render ()
drawLayer w h p (Layer Point a) = drawPoints $ zip3 (map ((*w) . scaleX p . x a) $ points p) (map ((*h) . scaleY p . y a) $ points p) (colour a <$> points p)
drawLayer _ _ _ _ = undefined -- (Layer Line p) = drawLines p

drawPoints :: [(Double,Double,Colour)] -> Render ()
drawPoints pts = do
  liftIO $ putStrLn $ "Drawing points " ++ show pts
  forM_ pts $ \(x,y,(r,g,b)) -> do
    arc x y 3 0 (2*pi)
    setSourceRGB r g b
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
