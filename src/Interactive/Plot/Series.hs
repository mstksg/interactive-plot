{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

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
    AutoPointStyle(..), apsMarker, apsColor
  , AutoSeries(..), asItems, asStyle
  , defaultStyles
  -- * Create a 'Series' from an 'AutoSeries'.
  , fromAutoSeries
  , fromAutoSeriesIO
  , fromAutoSeries_
  -- * Create common 'Series'
  , listSeries
  , tupleSeries
  , autoSeries
  , funcSeries
  , enumRange
  ) where

import           Control.Monad.Random
import           Control.Monad.State
import           Data.Default
import           Data.Foldable
import           Data.Maybe
import           Graphics.Vty
import           Interactive.Plot.Core
import           Lens.Micro
import           Lens.Micro.TH
import qualified Data.Map              as M
import qualified Data.Set              as S

-- | A version of 'PointStyle' where you can leave the marker or color
-- blank, to be automatically inferred.
data AutoPointStyle = APS
    { _apsMarker :: Maybe Char
    , _apsColor  :: Maybe Color
    }
  deriving Show

makeLenses ''AutoPointStyle

instance Default AutoPointStyle where
    def = APS Nothing Nothing

-- | A version of 'Series' where you can leave the marker or color blank,
-- to be automatically inferred.
data AutoSeries = AS { _asItems :: M.Map Double (S.Set Double)
                     , _asStyle :: AutoPointStyle
                     }
  deriving Show

makeLenses ''AutoSeries

-- | Construct a series from any foldable container of y-values.
listSeries :: Foldable t => t Double -> AutoPointStyle -> AutoSeries
listSeries xs = AS (toCoordMap . S.fromList . zipWith C [0..] . toList $ xs)

-- | Construct a series from any foldable container of x-y tuples.
tupleSeries :: Foldable t => t (Double, Double) -> AutoPointStyle -> AutoSeries
tupleSeries xs = AS (toCoordMap . S.fromList . foldMap ((:[]) . uncurry C) $ xs)

-- | Convert from a 'Series' back into an 'AutoSeries' with settings given.
autoSeries :: Series -> AutoSeries
autoSeries s = AS (s ^. sItems)
             $ APS (Just (s ^. sStyle . psMarker))
                   (Just (s ^. sStyle . psColor))

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
funcSeries
    :: Foldable t
    => (Double -> Double)
    -> t Double
    -> AutoPointStyle
    -> AutoSeries
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
fromAutoSeries = fromAutoSeries_ $ mkStdGen 28922710942259

-- | Turn an 'AutoSeries' into a 'Series', drawing styles randomly in IO.
fromAutoSeriesIO :: [AutoSeries] -> IO [Series]
fromAutoSeriesIO as = (`fromAutoSeries_` as) <$> getStdGen

-- | Turn an 'AutoSeries' into a 'Series', shuffling the default styles in
-- a deterministic way from a given seed.
fromAutoSeries_ :: StdGen -> [AutoSeries] -> [Series]
fromAutoSeries_ seed = flip evalRand seed . flip evalStateT S.empty . mapM go
  where
    go :: AutoSeries -> StateT (S.Set PointStyle) (Rand StdGen) Series
    go (AS is ps) = Series is <$> pickPs
      where
        pickPs = case ps of
          APS Nothing Nothing -> do
            picked <- get
            samp <- sampleSet $ defaultStyles S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet defaultStyles
              Just s  -> s <$ put (s `S.insert` picked)
          APS (Just m) Nothing  -> do
            picked <- get
            let allDefaults = combinePointStyles (S.singleton m) defaultColors
            samp <- sampleSet $ allDefaults S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet allDefaults
              Just s  -> s <$ put (s `S.insert` picked)
          APS Nothing (Just c) -> do
            picked <- get
            let allDefaults = combinePointStyles defaultMarkers (S.singleton (OC c))
            samp <- sampleSet $ allDefaults S.\\ picked
            case samp of
              Nothing -> fromJust <$> sampleSet allDefaults
              Just s  -> s <$ put (s `S.insert` picked)
          APS (Just m) (Just c) -> pure $ PointStyle m c

sampleSet
    :: (MonadRandom m)
    => S.Set a
    -> m (Maybe a)
sampleSet xs
    | S.null xs = pure Nothing
    | otherwise = do
        i <- getRandomR (0, S.size xs - 1)
        pure $ Just (i `S.elemAt` xs)

