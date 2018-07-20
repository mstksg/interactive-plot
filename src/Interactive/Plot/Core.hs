{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive.Plot.Core (
    Coord(..), Range(..), PointStyle(..), Series(..), Alignment(..), PlotOpts(..)
  , renderPlot
  , scaleRange, rSize
  , OrdColor(..)
  , plotRange
  ) where

import           Control.Applicative
import           Data.Default
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Coerce
import           Graphics.Vty
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


data Coord a = C { cX :: a
                 , cY :: a
                 }
  deriving (Show, Functor, Foldable)

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

data Range a = R { rMin :: a
                 , rMax :: a
                 }
  deriving (Show, Functor, Foldable)

instance Applicative Range where
    pure x = R x x
    R f g <*> R x y = R (f x) (g y)

rSize :: Num a => Range a -> a
rSize R{..} = rMax - rMin

data PointStyle = PointStyle { psMarker :: Char
                             , psColor  :: Color
                             }
  deriving (Eq)

instance Ord PointStyle where
    compare (PointStyle m1 c1) (PointStyle m2 c2)
        = compare m1 m2 <> compareColor c1 c2
      where
        compareColor = \case
          ISOColor c -> \case
            ISOColor d -> compare c d
            Color240 _ -> LT
          Color240 c -> \case
            ISOColor _ -> GT
            Color240 d -> compare c d

data Series = Series { sItems :: [Coord Double]
                     , sStyle :: PointStyle
                     }

data Alignment = ALeft
               | ACenter
               | ARight

-- data RangeRatio = RR { -- | Where on the screen (0 to 1) to place the other axis
--                        rrZero  :: Double
--                        -- | Ratio of height of a terminal character to width
--                      , rrRatio :: Double
--                      }
--                 deriving (Show)

data PlotOpts = PO { poTermRatio   :: Double            -- ^ character width ratio of terminal (H/W)
                   , poAspectRatio :: Maybe Double      -- ^ plot aspect ratio (H/W)
                   , poXRange      :: Maybe (Range Double)
                   , poYRange      :: Maybe (Range Double)
                   }

instance Default PlotOpts where
    def = PO { poTermRatio   = 2.1
             , poAspectRatio = Just 1
             , poXRange      = Nothing
             , poYRange      = Nothing
             }


plotRange
    :: PlotOpts
    -> Coord (Range Int)      -- ^ display region
    -> [Series]               -- ^ Points
    -> Coord (Range Double)   -- ^ actual plot axis range
plotRange PO{..} dr ss = case poAspectRatio of
    Just rA ->
      let displayRatio = fromIntegral (rSize (cY dr))
                       / (fromIntegral (rSize (cX dr)) * poTermRatio)
                       * rA
      in  case (poXRange, poYRange) of
            (Nothing, Nothing) -> case compare pointRangeRatio displayRatio of
              LT -> C (setRangeSize (rSize (cY pointRange) / displayRatio) (cX pointRange))
                      (cY pointRange)
              EQ -> pointRange
              GT -> C (cX pointRange)
                      (setRangeSize (rSize (cX pointRange) * displayRatio) (cY pointRange))
            (Just x , Nothing) -> C x (setRangeSize (rSize x * displayRatio) $ cY pointRange)
            (Nothing, Just y ) -> C (setRangeSize (rSize y / displayRatio) $ cX pointRange) y
            (Just x , Just y ) -> C x y
    Nothing -> case (poXRange, poYRange) of
      (Nothing, Nothing) -> pointRange
      (Just x , Nothing) -> C x (cY pointRange)
      (Nothing, Just y ) -> C (cX pointRange) y
      (Just x , Just y ) -> C x y
  where
    unZero :: Range Double -> Range Double
    unZero r
      | rSize r == 0 = R (subtract 1) (+ 1) <*> r
      | otherwise    = r
    pointRangeRatio :: Double
    pointRangeRatio = rSize (cY pointRange) / rSize (cX pointRange)
    pointRange :: Coord (Range Double)
    pointRange = fmap unZero
               . foldl' (liftA2 go) (C (R 0 0) (R 0 0))
               $ concatMap sItems ss
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
    xAxis  = placeImage dr pr (C ALeft   ACenter) (C (rMin (cX pr)) 0             ) $
                charFill defAttr '-' (rSize (cX dr)) 1
    yAxis  = placeImage dr pr (C ACenter ALeft  ) (C 0              (rMax (cY pr))) $
                charFill defAttr '|' 1               (rSize (cY dr))
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
placeImage dr pr (C aX aY) r i = translate x' (rSize (cY dr) - y') i
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
    rMin rNew + (x - rMin rOld) / rSize rOld * rSize rNew

scaleRange :: Fractional a => a -> Range a -> Range a
scaleRange x r = lerp unit r . (* x) . lerp r unit <$> r
  where
    unit = R (-1) 1

setRangeSize :: Fractional a => a -> Range a -> Range a
setRangeSize x r = lerp unit r <$> R (-x/2) (x/2)
  where
    unit = R (-1) 1

renderSeries
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Double)     -- ^ Plot axis range
    -> Series                   -- ^ Series to plot
    -> [Image]
renderSeries dr pr Series{..} = map go sItems
  where
    go :: Coord Double -> Image
    go r = placeImage dr pr (C ACenter ACenter) r $ renderPoint sStyle

renderPoint
    :: PointStyle
    -> Image
renderPoint PointStyle{..} = char (defAttr `withForeColor` psColor) psMarker
