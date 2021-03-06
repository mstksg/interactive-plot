{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Interative.Plot.Series
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Create common serieses.
module Interactive.Plot.Series (
    Series, AutoSeries, SeriesF(..)
  , PointStyle, AutoPointStyle, PointStyleF(..)
  -- * Create common 'Series'
  , listSeries
  , tupleSeries
  , funcSeries
  , enumRange
  -- * Create a 'Series' from an 'AutoSeries'.
  , fromAutoSeries
  , fromAutoSeriesIO
  , fromAutoSeries_
  , defaultStyles
  ) where

import           Control.Monad.Random
import           Control.Monad.State
import           Data.Foldable
import           Data.Maybe
import           Graphics.Vty
import           Interactive.Plot.Core
import           Lens.Micro
import qualified Data.Set              as S

-- | Construct a series from any foldable container of y-values.  The
-- x-values are automatically assigned to 0, 1, 2, 3 ... etc.
--
-- Note that this is polymorphic over both 'PointStyle' and
-- 'AutoPointStyle':
--
-- @
-- 'listSeries' :: Foldable t => t Double -> 'PointStyle' -> 'Series'
-- 'listSeries' :: Foldable t => t Double -> 'AutoPointStyle' -> 'AutoSeries'
-- @
listSeries :: Foldable t => t Double -> PointStyleF f -> SeriesF f
listSeries xs = Series (toCoordMap . S.fromList . zipWith C [0..] . toList $ xs)

-- | Construct a series from any foldable container of x-y tuples.
--
-- Note that this is polymorphic over both 'PointStyle' and
-- 'AutoPointStyle':
--
-- @
-- 'tupleSeries' :: Foldable t => t (Double, Double) -> 'PointStyle' -> 'Series'
-- 'tupleSeries' :: Foldable t => t (Double, Double) -> 'AutoPointStyle' -> 'AutoSeries'
-- @
tupleSeries :: Foldable t => t (Double, Double) -> PointStyleF f -> SeriesF f
tupleSeries xs = Series (toCoordMap . S.fromList . foldMap ((:[]) . uncurry C) $ xs)

-- | @'enumRange' n ('R' a b)@ generates a list of @n@ equally spaced values
-- between @a@ and @b@.
enumRange
    :: Fractional a
    => Int       -- ^ Number of points
    -> Range a   -- ^ Range to generate the points over
    -> [a]
enumRange n r = (+ r ^. rMin) . (* s) . fromIntegral <$> [0 .. (n - 1)]
  where
    s = r ^. rSize / fromIntegral (n - 1)

-- | Construct a series from a function x to y, given a foldable container
-- of x values.
--
-- Note that this is polymorphic over both 'PointStyle' and
-- 'AutoPointStyle':
--
-- @
-- 'funcSeries' :: Foldable t => (Double -> Double) -> t Double -> 'PointStyle' -> 'Series'
-- 'funcSeries' :: Foldable t => (Double -> Double) -> t Double -> 'AutoPointStyle' -> 'AutoSeries'
-- @
funcSeries
    :: Foldable t
    => (Double -> Double)
    -> t Double
    -> PointStyleF f
    -> SeriesF f
funcSeries f xs = tupleSeries [ (x, f x) | x <- toList xs ]

-- | A set of default markers.
defaultMarkers :: S.Set Char
defaultMarkers = S.fromList "o*+~.,=#`x-"

-- | A set of default colors.
defaultColors :: S.Set OrdColor
defaultColors = S.fromList $ OC <$> [white, yellow, blue, red, green, cyan, magenta]

-- | A set of default point styles
defaultStyles :: S.Set PointStyle
defaultStyles = combinePointStyles defaultMarkers defaultColors

combinePointStyles
    :: S.Set Char
    -> S.Set OrdColor
    -> S.Set PointStyle
combinePointStyles ms cs = combine `S.map` S.cartesianProduct ms cs
  where
    combine (m, OC c) = PointStyle m c

-- | Turn an 'AutoSeries' into a 'Series', assigning styles from
-- a pre-specified "shuffled" order.
fromAutoSeries :: [AutoSeries] -> [Series]
fromAutoSeries = fromAutoSeries_ $
    fromMaybe (mkStdGen 28922710942259) (_poAutoMethod defaultPlotOpts)

-- | Turn an 'AutoSeries' into a 'Series', drawing styles randomly in IO.
fromAutoSeriesIO :: [AutoSeries] -> IO [Series]
fromAutoSeriesIO as = (`fromAutoSeries_` as) <$> getStdGen

-- | Turn an 'AutoSeries' into a 'Series', shuffling the default styles in
-- a deterministic way from a given seed.
fromAutoSeries_ :: StdGen -> [AutoSeries] -> [Series]
fromAutoSeries_ seed = flip evalRand seed . flip evalStateT S.empty . mapM go
  where
    go :: AutoSeries -> StateT (S.Set PointStyle) (Rand StdGen) Series
    go (Series is ps) = Series is <$> pickPs
      where
        pickPs = case ps of
          PointStyleF Auto Auto -> do
            picked <- get
            samp <- sampleSet $ defaultStyles S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet defaultStyles
              Just s  -> s <$ put (s `S.insert` picked)
          PointStyleF (Given m) Auto -> do
            picked <- get
            let allDefaults = combinePointStyles (S.singleton m) defaultColors
            samp <- sampleSet $ allDefaults S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet allDefaults
              Just s  -> s <$ put (s `S.insert` picked)
          PointStyleF Auto (Given c) -> do
            picked <- get
            let allDefaults = combinePointStyles defaultMarkers (S.singleton (OC c))
            samp <- sampleSet $ allDefaults S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet allDefaults
              Just s  -> s <$ put (s `S.insert` picked)
          PointStyleF (Given m) (Given c) -> pure $ PointStyle m c

sampleSet
    :: (MonadRandom m)
    => S.Set a
    -> m (Maybe a)
sampleSet xs
    | S.null xs = pure Nothing
    | otherwise = do
        i <- getRandomR (0, S.size xs - 1)
        pure $ Just (i `S.elemAt` xs)

