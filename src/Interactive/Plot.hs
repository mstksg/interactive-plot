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
-- Simple interactive rendering of plots.  See README for information on
-- usage.
--
-- The main way to use this library is to use 'runPlotAuto' or 'runPlot' on
-- some series you make using the series constructors ('listSeries',
-- 'funcSeries', etc.)
--
module Interactive.Plot (
  -- * Construct Series
    Auto(..)
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
  -- * Run a Plot
  , runPlot
  , runPlotAuto
  -- ** Animated
  , animatePlot, lastForever
  , animatePlotFunc
  -- ** Options
  , PlotOpts(..), poTermRatio, poAspectRatio, poXRange, poYRange, poRange, poAutoMethod, poHelp, poFramerate, poDelay
  , defaultPlotOpts
  ) where

import           Interactive.Plot.Core
import           Interactive.Plot.Run
import           Interactive.Plot.Series
