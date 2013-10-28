-- |
-- Module:      Graphics.HsPlot.Render
-- Copyright:   (c) 2013, Regents of the University of California.
-- License:     BSD3
-- 
-- Maintainer:  Émilien Tlapale <emilien@tlapale.com>
-- Stability:   experimental
-- Portability: portable
-- 
-- Generic rendering functions.

module Graphics.HsPlot.Render (
  PlotPoint(..), plotPoints
)
where

type PlotPoint = (Double, Double, Colour, Double, Shape)

-- |Convert a points in a Layer to a Traversable of PlotPoint's.
plotPoints :: (Traversable f)
           => Plot f a x y c p s h
           -> Maybe (Aesthetics a x y c p s h)
           -> [PlotPoint]
plotPoints p aes = tZip5
  (fmap (scaleMap (scaleX p) . x a) $ points p)
  (fmap (scaleMap (scaleY p) . y a) $ points p)
  (fmap (scaleMap (scaleC p) . colour a) $ points p)
  (fmap (scaleMap (scaleP p) . alpha a) $ points p)
  (fmap (scaleMap (scaleH p) . shape a) $ points p)
  where a = fromMaybe (aesthetics p) aes

tZip5 :: Traversable f
      => f a -> f b -> f c -> f d -> f e-> [(a,b,c,d,e)]
tZip5 m n o p q = zip5 (toList m) (toList n) (toList o) (toList p) (toList q)

zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : zip5 as bs cs ds es
zip5 _      _      _      _  _     = []
