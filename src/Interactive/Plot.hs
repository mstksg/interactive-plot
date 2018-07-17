{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive.Plot (
    Coord(..), Range(..), PointStyle(..), Series(..), Alignment(..), RangeRatio(..), PlotRange(..)
  , renderPlot
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Compose
import           Graphics.Vty
import           Text.Printf

data Coord a = C { cX :: a
                 , cY :: a
                 }
  deriving (Functor, Foldable)

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
  deriving (Functor, Foldable)

instance Applicative Range where
    pure x = R x x
    R f g <*> R x y = R (f x) (g y)

rSize :: Num a => Range a -> a
rSize R{..} = rMax - rMin

data PointStyle = PointStyle { psMarker :: Char
                             , psColor  :: Color
                             }

data Series = Series { sItems :: [Coord Double]
                     , sStyle :: PointStyle
                     }

data Alignment = ALeft
               | ACenter
               | ARight

data RangeRatio = RR { -- | Where on the screen (0 to 1) to place the other axis
                       rrZero  :: Double
                       -- | Ratio of height of a terminal character to width
                     , rrRatio :: Double
                     }
                deriving (Show)

data PlotRange = PRXY (Coord (Range Double))
               | PRX  (Range Double) RangeRatio
               | PRY  RangeRatio     (Range Double)

plotRange
    :: Coord (Range Int)      -- ^ display region
    -> PlotRange              -- ^ plot axis range specification
    -> Coord (Range Double)   -- ^ actual plot axis range
plotRange dr = \case
    PRXY pr        -> pr
    PRX  rX RR{..} ->
      let yr = rSize rX * fromIntegral (rSize (cY dr)) / fromIntegral (rSize (cX dr)) * rrRatio
          y0 = (rrZero - 1) * yr
      in  C rX (R y0 (y0 + yr))
    PRY  RR{..} rY ->
      let xr = rSize rY * fromIntegral (rSize (cY dr)) / fromIntegral (rSize (cX dr)) * rrRatio
          x0 = (rrZero - 1) * xr
      in  C (R x0 (x0 + xr)) rY

renderPlot
    :: Coord (Range Int)        -- ^ display region
    -> PlotRange                -- ^ plot axis range spec
    -> [Series]
    -> [Image]
renderPlot dr pr ss = concatMap (renderSeries dr pr') ss
                   ++ renderAxis dr pr'
  where
    pr' = plotRange dr pr

renderAxis
    :: Coord (Range Int)        -- ^ display region
    -> Coord (Range Double)     -- ^ plot axis range
    -> [Image]
renderAxis dr pr = foldMap toList axisBounds ++ axisLines
  where
    origin = char defAttr '+'
    xAxis  = charFill defAttr '-' (rSize (cX dr)) 1
    yAxis  = charFill defAttr '|' 1               (rSize (cY dr))
    axisLines = placeImage dr pr (C ACenter ACenter) (C 0 0) <$>
                     [origin, xAxis, yAxis]
    axisBounds :: Coord (Range Image)
    axisBounds = getCompose $ do
      pos    <- Compose pr
      coords <- Compose $ C (pure $ \d -> C d 0) (pure $ \d -> C 0 d)
      xAlign <- Compose $ C (R ALeft   ARight ) (R ACenter ACenter)
      yAlign <- Compose $ C (R ACenter ACenter) (R ARight  ALeft  )
      pure $ placeImage dr pr (C xAlign yAlign) (coords pos) (string defAttr $  printf "%.2f" pos)

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
renderPoint (PointStyle{..}) = char (defAttr `withForeColor` psColor) psMarker
