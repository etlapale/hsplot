-- |
-- Module:      Graphics.HsPlot.Cairo
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- Cairo backend to HsPlot.

module Graphics.HsPlot.Cairo (
  renderToPNG, renderToPDF, render
)
where

import Control.Applicative
import Data.Foldable as Foldable
import Data.Traversable

import Graphics.Rendering.Cairo

import Graphics.HsPlot.Base

renderToPNG :: (Applicative f, Traversable f, Show x, Show y)
            => Int -> Int -> FilePath -> Plot f a x y c p s h -> IO ()
renderToPNG w h f p =
  withImageSurface FormatARGB32 w h $ \s -> do
    renderWith s $ render (realToFrac w) (realToFrac h) p
    surfaceWriteToPNG s f

renderToPDF :: (Applicative f, Traversable f, Show x, Show y)
            => Double -> Double -> FilePath -> Plot f a x y c p s h -> IO ()
renderToPDF w h f p =
  withPDFSurface f w h $ \s -> do
    renderWith s $ render w h p
    
render :: (Applicative f, Traversable f, Show x, Show y)
       => Double -> Double -> Plot f a x y c p s h -> Render ()
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
        lw = w - ofx
        lh = h - ofy
        tickSize = 5
        xticksmargin = tickSize/2    -- Vertical margin on top of tick labels
        yticksmargin = tickSize      -- Horizontal margin right of tick labels

drawBackground :: Render ()
drawBackground = do
  setSourceRGB 1 1 1
  paint

drawLayer :: (Traversable f, Applicative f)
          => Double -> Double -> Plot f a x y c p s h -> Layer a x y c p s h -> Render ()
drawLayer w h p (Layer Point a) =
  forM_ (plotPoints p a) $ \(x,y,c,p,sh) -> drawPoint (x*w,y*h,c,p,sh)
drawLayer _ _ _ _ = undefined -- (Layer Line p) = drawLines p

drawPoint :: PlotPoint -> Render ()
drawPoint (x,y,(r,g,b),a,sh) = do
  liftIO $ putStrLn $ "Point alpha: " ++ show a
  setSourceRGBA r g b a
  setLineWidth 1.0
  drawShape x y sz sh
  where sz = 4

drawShape :: Double -> Double -> Double -> Shape -> Render ()
drawShape x y sz Circle = do
  arc x y (sz/2) 0 (2*pi)
  fill
drawShape x y sz Triangle = do
  moveTo x (y-2*sz/3)
  relLineTo (sz/2) sz
  relLineTo (-sz) 0
  closePath
  fill
drawShape x y sz Square = do
  rectangle (x-sz/2) (y-sz/2) sz sz
  fill
drawShape x y sz Cross = do
  moveTo x (y-sz/2)
  relLineTo 0 sz
  moveTo (x-sz/2) y
  relLineTo sz 0
  stroke
drawShape x y sz DiagSquare = do
  moveTo (x-sz/2) (y-sz/2)
  relLineTo sz sz
  moveTo (x+sz/2) (y-sz/2)
  relLineTo (-sz) sz
  stroke
drawShape x y sz _ = do
  arc x y (sz/4) 0 (2*pi)
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
