{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Interactive.Plot.Core (
    Coord(..), cX, cY
  , Range(.., R2), rMin, rMax, rSize, rMid, _rSize
  , PointStyle(..), psMarker, psColor
  , Series(..), sItems, sStyle
  , Alignment(..)
  , PlotOpts(..), poTermRatio, poAspectRatio, poXRange, poYRange
  , renderPlot
  , OrdColor(..)
  , plotRange
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Coerce
import           Data.Default
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Maybe
import           Data.Ord
import           Graphics.Vty
import           Lens.Micro
import           Lens.Micro.TH
import           Text.Printf

newtype OrdColor = OC { getOC :: Color }
    deriving Eq

instance Ord OrdColor where
    compare = coerce compareColor
      where
        compareColor = \case
          ISOColor c -> \case
            ISOColor d -> compare c d
            Color240 _ -> LT
          Color240 c -> \case
            ISOColor _ -> GT
            Color240 d -> compare c d

data Coord a = C { _cX :: a
                 , _cY :: a
                 }
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Coord

instance Num a => Num (Coord a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Applicative Coord where
    pure x = C x x
    C f g <*> C x y = C (f x) (g y)

data Range a = R { _rMin :: a
                 , _rMax :: a
                 }
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Range

instance Applicative Range where
    pure x = R x x
    R f g <*> R x y = R (f x) (g y)

data PointStyle = PointStyle { _psMarker :: Char
                             , _psColor  :: Color
                             }
  deriving (Eq)

makeLenses ''PointStyle

instance Ord PointStyle where
    compare = comparing $ \case PointStyle m1 c1 -> (m1, OC c1)

data Series = Series { _sItems :: [Coord Double]
                     , _sStyle :: PointStyle
                     }

makeLenses ''Series

data Alignment = ALeft
               | ACenter
               | ARight

data PlotOpts = PO { _poTermRatio   :: Double            -- ^ character width ratio of terminal (H/W)
                   , _poAspectRatio :: Maybe Double      -- ^ plot aspect ratio (H/W)
                   , _poXRange      :: Maybe (Range Double)
                   , _poYRange      :: Maybe (Range Double)
                   }

makeLenses ''PlotOpts

instance Default PlotOpts where
    def = PO { _poTermRatio   = 2.1
             , _poAspectRatio = Just 1
             , _poXRange      = Nothing
             , _poYRange      = Nothing
             }

pattern R2 :: Fractional a => a -> a -> Range a
pattern R2 rM rS <- (\case R{..} -> ((_rMin + _rMax) / 2, _rMax - _rMin)->(rM, rS))
  where
    R2 rM rS = R (rM - rS2) (rM + rS2)
      where
        rS2 = rS / 2
{-# COMPLETE R2 #-}

_rSize :: Num a => Range a -> a
_rSize R{..} = _rMax - _rMin

rSize :: Fractional a => Lens' (Range a) a
rSize f (R2 m s) = R2 m <$> f s

rMid :: Fractional a => Lens' (Range a) a
rMid f (R2 m s) = (`R2` s) <$> f m

within :: Ord a => a -> Range a -> Bool
within x r = x >= r ^. rMin && x <= r ^. rMax

plotRange
    :: PlotOpts
    -> Coord (Range Int)      -- ^ display region
    -> [Series]               -- ^ Points
    -> Coord (Range Double)   -- ^ actual plot axis range
plotRange PO{..} dr ss = case _poAspectRatio of
    Just rA ->
      let displayRatio = fromIntegral (dr ^. cY . to _rSize)
                       / (fromIntegral (dr ^. cX . to _rSize) / _poTermRatio)
                       * rA
      in  case (_poXRange, _poYRange) of
            (Nothing, Nothing) -> case compare pointRangeRatio displayRatio of
              LT -> pointRange
                      & cY . rSize .~ pointRange ^. cX . rSize * displayRatio
              EQ -> pointRange
              GT -> pointRange
                      & cX . rSize .~ pointRange ^. cY . rSize / displayRatio
            (Just x , Nothing) -> pointRange
                                    & cX .~ x
                                    & cY . rSize .~ x ^. rSize * displayRatio
            (Nothing, Just y ) -> pointRange
                                    & cX . rSize .~ y ^. rSize / displayRatio
                                    & cY .~ y
            (Just x , Just y ) -> C x y
    Nothing -> case (_poXRange, _poYRange) of
      (Nothing, Nothing) -> pointRange
      (Just x , Nothing) -> pointRange & cX .~ x
      (Nothing, Just y ) -> pointRange & cY .~ y
      (Just x , Just y ) -> C x y
  where
    unZero :: Range Double -> Range Double
    unZero r
      | r ^. rSize == 0 = R (subtract 1) (+ 1) <*> r
      | otherwise       = r
    pointRangeRatio :: Double
    pointRangeRatio = pointRange ^. cY . rSize / pointRange ^. cX . rSize
    pointRange :: Coord (Range Double)
    pointRange = fmap unZero
               . foldl' (liftA2 go) (C (R 0 0) (R 0 0))
               $ concatMap _sItems ss
      where
        go oldR x = R min max <*> pure x <*> oldR

renderPlot
    :: Coord (Range Int)        -- ^ display region
    -> Coord (Range Double)     -- ^ plot axis range
    -> [Series]
    -> [Image]
renderPlot dr pr = overlayAxis dr pr
                 . concatMap (renderSeries dr pr)

overlayAxis
    :: Coord (Range Int)        -- ^ display region
    -> Coord (Range Double)     -- ^ plot axis range
    -> [Image]                  -- ^ thing to overlay over
    -> [Image]
overlayAxis dr pr is = foldMap toList axisBounds ++ is ++ axisLines
  where
    origin = placeImage dr pr (C ACenter ACenter) (C 0 0) $
                char defAttr '+'
    xAxis  = placeImage dr pr (C ALeft   ACenter) (C (pr ^. cX . rMin) 0                ) $
                charFill defAttr '-' (dr ^. cX . to _rSize) 1
    yAxis  = placeImage dr pr (C ACenter ALeft  ) (C 0                 (pr ^. cY . rMax)) $
                charFill defAttr '|' 1                  (dr ^. cY . to _rSize)
    axisLines = [origin, xAxis, yAxis]
    axisBounds :: Coord (Range Image)
    axisBounds = getCompose $ do
      pos    <- Compose pr
      coords <- Compose $ C (pure $ \d -> C d 0) (pure $ \d -> C 0 d)
      xAlign <- Compose $ C (R ALeft   ARight ) (R ACenter ACenter)
      yAlign <- Compose $ C (R ACenter ACenter) (R ARight  ALeft  )
      pure $ placeImage dr pr (C xAlign yAlign) (coords pos)
                (string defAttr $  printf "%.2f" pos)

placeImage
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Double)     -- ^ Plot axis range
    -> Coord Alignment          -- ^ Alignment
    -> Coord Double             -- ^ Position in plot space
    -> Image                    -- ^ Image to place
    -> Image
placeImage dr pr (C aX aY) r i = translate x' (dr ^. cY . to _rSize - y') i
  where
    dr' = (fmap . fmap) fromIntegral dr
    scaled  = lerp <$> pr <*> dr' <*> r
    C x' y' = (round <$> scaled) + C (aligner aX (imageWidth  i))
                                     (negate (aligner aY (imageHeight i)))
    aligner = \case
      ALeft   -> const 0
      ACenter -> negate . (`div` 2)
      ARight  -> negate

lerp
    :: Fractional a
    => Range a        -- ^ input range
    -> Range a        -- ^ output range
    -> a
    -> a
lerp rOld rNew x =
    rNew ^. rMin + (x - rOld ^. rMin) / (rOld ^. rSize) * (rNew ^. rSize)

renderSeries
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Double)     -- ^ Plot axis range
    -> Series                   -- ^ Series to plot
    -> [Image]
renderSeries dr pr Series{..} = mapMaybe go _sItems
  where
    go :: Coord Double -> Maybe Image
    go r = placeImage dr pr (C ACenter ACenter) r (renderPoint _sStyle)
              <$ guard (and $ within <$> r <*> pr)

renderPoint
    :: PointStyle
    -> Image
renderPoint PointStyle{..} = char (defAttr `withForeColor` _psColor) _psMarker
