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
  module HsPlot
)
where

import Graphics.HsPlot.Base as HsPlot
import Graphics.HsPlot.Cairo as HsPlot
import Graphics.HsPlot.Colours as HsPlot
import Graphics.HsPlot.Render as HsPlot
import Graphics.HsPlot.Scales as HsPlot
