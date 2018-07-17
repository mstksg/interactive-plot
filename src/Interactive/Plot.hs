{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive.Plot (
    Coord(..), Range(..), PointStyle(..), Series(..), Alignment(..)
  , renderPlot
  ) where

import           Control.Applicative
import           Graphics.Vty

data Coord a = C { cX :: a
                 , cY :: a
                 }
  deriving (Functor)

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
  deriving (Functor)

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

renderPlot
    :: Coord (Range Int)        -- ^ display region
    -> Coord (Range Int)        -- ^ plot axis range
    -> [Series]
    -> [Image]
renderPlot dr pr ss = concatMap (renderSeries dr pr) ss
                   ++ renderAxis dr pr

renderAxis
    :: Coord (Range Int)        -- ^ display region
    -> Coord (Range Int)        -- ^ plot axis range
    -> [Image]
renderAxis dr pr = placeImage dr pr (C ACenter ACenter) (C 0 0) <$>
                     [origin, xAxis, yAxis]
  where
    origin = char defAttr '+'
    xAxis  = charFill defAttr '-' (rSize (cX dr)) 1
    yAxis  = charFill defAttr '|' 1               (rSize (cY dr))

placeImage
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Int)        -- ^ Plot axis range
    -> Coord Alignment          -- ^ Alignment
    -> Coord Double             -- ^ Position in plot space
    -> Image                    -- ^ Image to place
    -> Image
placeImage dr pr (C aX aY) r i = translate x' (rSize (cY dr) - y') i
  where
    dr' = (fmap . fmap) fromIntegral dr
    pr' = (fmap . fmap) fromIntegral pr
    scaled  = lerp <$> pr' <*> dr' <*> r
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
    -> Coord (Range Int)        -- ^ Plot axis range
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
