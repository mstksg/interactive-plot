{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Interactive.Plot.Series (
    AutoPointStyle(..), apsMarker, apsColor
  , AutoSeries(..), asItems, asStyle
  , defaultStyles
  , fromAutoSeries
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
import qualified Data.Set              as S

data AutoPointStyle = APS
    { _apsMarker :: Maybe Char
    , _apsColor  :: Maybe Color
    }
  deriving Show

makeLenses ''AutoPointStyle

instance Default AutoPointStyle where
    def = APS Nothing Nothing

data AutoSeries = AS { _asItems :: [Coord Double]
                     , _asStyle :: AutoPointStyle
                     }
  deriving Show

makeLenses ''AutoSeries

listSeries :: Foldable t => t Double -> AutoPointStyle -> AutoSeries
listSeries xs = AS (zipWith C [0..] (toList xs))

tupleSeries :: Foldable t => t (Double, Double) -> AutoPointStyle -> AutoSeries
tupleSeries xs = AS (uncurry C <$> toList xs)

autoSeries :: Series -> AutoSeries
autoSeries (Series xs PointStyle{..}) = AS xs $ APS (Just _psMarker) (Just _psColor)

enumRange :: Int -> Range Double -> [Double]
enumRange n r = (+ r ^. rMin) . (* s) . fromIntegral <$> [0 .. (n - 1)]
  where
    s = r ^. rSize / fromIntegral (n - 1)

funcSeries :: Foldable t => (Double -> Double) -> t Double -> AutoPointStyle -> AutoSeries
funcSeries f xs = tupleSeries [ (x, f x) | x <- toList xs ]

defaultMarkers :: S.Set Char
defaultMarkers = S.fromList "o*+~.,=#`x-"
defaultStyles :: S.Set PointStyle
defaultColors = S.fromList $ OC <$> [white, yellow, blue, red, green, cyan, magenta]
defaultColors :: S.Set OrdColor
defaultStyles = combinePointStyles defaultMarkers defaultColors

combinePointStyles
    :: S.Set Char
    -> S.Set OrdColor
    -> S.Set PointStyle
combinePointStyles ms cs = combine `S.map` S.cartesianProduct ms cs
  where
    combine (m, OC c) = PointStyle m c


fromAutoSeries :: [AutoSeries] -> [Series]
fromAutoSeries = flip evalRand seed . flip evalStateT S.empty . mapM go
  where
    seed = mkStdGen 28922710942259
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

