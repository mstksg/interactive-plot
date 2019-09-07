{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Interative.Plot
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Simple interactive rendering of plots
--
module Interactive.Plot (
  -- * Run a Plot
    runPlot
  , runPlotAuto
  -- ** Animated
  , animatePlot, lastForever
  , animatePlotFunc
  , animatePlotMoore, Moore(..)
  -- ** Options
  , PlotOpts(..), poTermRatio, poAspectRatio, poXRange, poYRange, poRange, poAutoMethod, poHelp, poFramerate, poDelay
  , defaultPlotOpts
  -- * Construct Series
  , Auto(..)
  , Series, AutoSeries, SeriesF(..), sItems, sStyle
  -- ** Making common serieses
  , listSeries
  , tupleSeries
  , funcSeries
  , enumRange
  , toCoordMap
  , fromCoordMap
  -- ** Series from AutoSeroes
  , fromAutoSeries
  , fromAutoSeriesIO
  , fromAutoSeries_
  -- ** Types
  , PointStyle, pattern PointStyle, _psMarker, _psColor, AutoPointStyle, PointStyleF(..), psMarker, psColor
  , Coord(..), cX, cY
  , Range(..), _rMid, _rSize', rMin, rMax, rSize, rMid, _rSize
  ) where

import           Interactive.Plot.Core
import           Interactive.Plot.Run
import           Interactive.Plot.Series
