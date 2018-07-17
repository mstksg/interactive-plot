{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Interactive.Plot (
  ) where

import           Graphics.Vty hiding                 ((<|>))

data Coord a = C { cX :: a
                 , cY :: a
                 }
  deriving (Functor)

instance Applicative Coord where
    pure x = C x x
    C f g <*> C x y = C (f x) (g y)

data Range a = R { rMin :: a
                 , rMax :: a
                 }
  deriving (Functor)

data PointStyle = PointStyle { psMarker :: Char
                             , psColor  :: Color
                             }

data Series = Series { sItems :: [Coord Double]
                     , sStyle :: PointStyle
                     }

data Alignment = ALeft
               | ACenter
               | ARight

placeImage
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Int)        -- ^ Plot axis range
    -> Coord Alignment          -- ^ Alignment
    -> Coord Double             -- ^ Position in plot space
    -> Image                    -- ^ Image to place
    -> Image
placeImage dr pr (C aX aY) r = realign . translate x' y'
  where
    dr' = (fmap . fmap) fromIntegral dr
    pr' = (fmap . fmap) fromIntegral pr
    C x' y' = fmap round $ lerp <$> dr' <*> pr' <*> r
    realign = translate <$> (aligner aX . imageWidth)
                        <*> (aligner aY . imageHeight)
                        <*> id
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
lerp (R oldMin oldMax) (R newMin newMax) x =
    newMin + (x - oldMin) / (oldMax - oldMin) * (newMax - newMin)

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
