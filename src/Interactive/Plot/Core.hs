{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- |
-- Module      : Interative.Plot.Core
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Core rendering functionality for the library.
--
module Interactive.Plot.Core (
    Coord(..), cX, cY
  , Range(.., RAbout), rMin, rMax, rSize, rMid, _rSize
  , Auto(..)
  , PointStyleF(.., PointStyle), PointStyle, AutoPointStyle, psMarker, psColor
  , SeriesF(..), Series, AutoSeries, sItems, sStyle, toCoordMap, fromCoordMap
  , Alignment(..)
  , PlotOpts(..), poTermRatio, poAspectRatio, poXRange, poYRange, poRange
  , renderPlot
  -- * Internal
  , plotRange
  , OrdColor(..)
  , renderPoint
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Coerce
import           Data.Default
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Ord
import           GHC.Generics          (Generic)
import           Graphics.Vty hiding   ((<|>))
import           Lens.Micro
import           Lens.Micro.TH
import           Text.Printf
import qualified Data.Map              as M
import qualified Data.Set              as S

-- | Newtype wrapper providing an 'Ord' instance for 'Color'.
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

-- | An ordered pair in @a@.
data Coord a = C { _cX :: a   -- ^ Access @x@.  For getter/setter lenses, see 'cX'.
                 , _cY :: a   -- ^ Access @y@.  For getter/setter lenses, see 'cY'.
                 }
  deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

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

-- | A specification for a range.  Using 'R', contains the minimum and
-- maximum.  Using 'RAbout', contains the midpoint and size.
data Range a = R { _rMin :: a     -- ^ Minimum of range.  For getter/setter lens, see 'rMin'.
                 , _rMax :: a     -- ^ Maximum of range.  For getter/setter lens, see 'rMax'.
                 }
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Range

-- | "Zipping" behavior on minimum and maximum
instance Applicative Range where
    pure x = R x x
    R f g <*> R x y = R (f x) (g y)

-- | Basically the same as @Reader 'Bool'@, for minimum and maximum
-- fields.
instance Monad Range where
    return x = R x x
    R x y >>= f = R (_rMin (f x)) (_rMax (f y))

data Auto a = Auto | Given a
  deriving (Show, Eq, Ord, Generic)

instance Semigroup (Auto a) where
    (<>) = \case
      Auto    -> id
      Given x -> \case Auto -> Given x; Given y -> Given y

instance Monoid (Auto a) where
    mempty = Auto

-- | Specification of a style for a point.
data PointStyleF f = PointStyleF
      { _psMarkerF :: f Char  -- ^ Marker cahracter.  For getter/setter lens, see 'psMarker'.
      , _psColorF  :: f Color -- ^ Marker color.  For getter/setter lens, see 'psColor'.
      }
  deriving (Generic)

makeLenses ''PointStyleF

type PointStyle     = PointStyleF Identity

-- | A version of 'PointStyle' where you can leave the marker or color
-- blank, to be automatically inferred.
type AutoPointStyle = PointStyleF Auto

pattern PointStyle :: Char -> Color -> PointStyle
pattern PointStyle { _psMarker, _psColor } = PointStyleF (Identity _psMarker) (Identity _psColor)
{-# COMPLETE PointStyle #-}

instance (Semigroup (f Char), Semigroup (f Color)) => Semigroup (PointStyleF f) where
    PointStyleF m1 c1 <> PointStyleF m2 c2 = PointStyleF (m1 <> m2) (c1 <> c2)
instance (Monoid (f Char), Monoid (f Color)) => Monoid (PointStyleF f) where
    mempty = PointStyleF mempty mempty

deriving instance (Eq (f Char), Eq (f Color)) => Eq (PointStyleF f)
instance (Ord (f Char), Ord (f OrdColor), Functor f, Eq (f Color)) => Ord (PointStyleF f) where
    compare = comparing $ \case PointStyleF m1 c1 -> (m1, OC <$> c1)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = Identity <$> f x

psMarker :: Lens' PointStyle Char
psMarker = psMarkerF . _Identity

psColor :: Lens' PointStyle Color
psColor = psColorF . _Identity

-- | Data for a single series: contains the coordinate map with the point
-- style for the series.
data SeriesF f = Series { _sItems :: M.Map Double (S.Set Double)
                        , _sStyle :: PointStyleF f
                        }


type Series = SeriesF Identity

-- | A version of 'Series' where you can leave the marker or color blank,
-- to be automatically inferred.
type AutoSeries = SeriesF Auto


makeLenses ''SeriesF

-- | Alignment specification.
data Alignment = ALeft
               | ACenter
               | ARight

data PlotOpts = PO
    { _poTermRatio   :: Double               -- ^ character width ratio of terminal (H/W)
    , _poAspectRatio :: Maybe Double         -- ^ plot aspect ratio (H/W)
    , _poXRange      :: Maybe (Range Double) -- ^ X Range.  Use 'Nothing' for automatic.
    , _poYRange      :: Maybe (Range Double) -- ^ Y Range.  Use 'Nothing' for automatic.
    }

makeLenses ''PlotOpts

instance Default PlotOpts where
    def = PO { _poTermRatio   = 2.1
             , _poAspectRatio = Just 1
             , _poXRange      = Nothing
             , _poYRange      = Nothing
             }

-- | An alternative "constructor" for 'R', which takes a midpoint and size
-- instead of a min and max.
pattern RAbout :: Fractional a => a -> a -> Range a
pattern RAbout { _rMid, _rSize' } <- (\case R{..} -> ((_rMin + _rMax) / 2, _rMax - _rMin)->(_rMid, _rSize'))
  where
    RAbout rM rS = R (rM - rS2) (rM + rS2)
      where
        rS2 = rS / 2
{-# COMPLETE RAbout #-}

-- | A version of '_rSize' that works for any instance of 'Num'.
_rSize :: Num a => Range a -> a
_rSize R{..} = _rMax - _rMin

-- | Lens into the size of a 'Range'  Modifying this size results in
-- a scaling about the midpoint of the range.
--
-- @
-- view rSize (R 2 4)
-- -- 2
-- over rSize (* 2) (R 2 4)
-- -- R 1 5
-- @
rSize :: Fractional a => Lens' (Range a) a
rSize f (RAbout m s) = RAbout m <$> f s

-- | Lens into the midpoint of a 'Range'.  Modifying this midpoint shifts
-- the range to a new midpoint, preserving the size.
--
-- @
-- view rMid (R 2 4)
-- -- 3
-- over rMid (+ 3) (R 2 4)
-- -- R 5 7
-- @
rMid :: Fractional a => Lens' (Range a) a
rMid f (RAbout m s) = (`RAbout` s) <$> f m

-- | Check if a point is within the 'Range' (inclusive).
within :: Ord a => a -> Range a -> Bool
within x r = x >= r ^. rMin && x <= r ^. rMax

-- | Lens into a 'PlotOpts' getting its range X and range Y settings.
poRange :: Lens' PlotOpts (Maybe (Range Double), Maybe (Range Double))
poRange f (PO r a x y) = uncurry (PO r a) <$> f (x, y)

-- | Compute plot axis ranges based on a list of points and the size of the
-- display region.
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
    -- TODO: can this be computed only for points in view?
    pointRange :: Coord (Range Double)
    pointRange = fmap unZero
               . foldl' (liftA2 go) (C (R 0 0) (R 0 0))
               $ ss ^.. traverse . sItems . folding fromCoordMap
      where
        go oldR x = R min max <*> pure x <*> oldR

-- | Render serieses based on a display region and plot axis ranges.
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

-- | Render a single series as a set of points.
renderSeries
    :: Coord (Range Int)        -- ^ Display region
    -> Coord (Range Double)     -- ^ Plot axis range
    -> Series                   -- ^ Series to plot
    -> [Image]
renderSeries dr pr Series{..} =
    M.foldMapWithKey (\x -> foldMap (maybeToList . go . C x))
        $ validPoints pr _sItems
  where
    go :: Coord Double -> Maybe Image
    go r = placeImage dr pr (C ACenter ACenter) r (renderPoint _sStyle)
              <$ guard (and $ within <$> r <*> pr)

validPoints
    :: Ord k
    => Coord (Range k)
    -> M.Map k (S.Set k)
    -> M.Map k (S.Set k)
validPoints pr = fmap (setRange (pr ^. cY))
               . mapRange (pr ^. cX)
  where
    mapRange r m = M.unions $ m''
                            : maybeToList (M.singleton (r ^. rMin) <$> mMin)
                           ++ maybeToList (M.singleton (r ^. rMax) <$> mMax)
      where
        (_  , mMin, m') = M.splitLookup (r ^. rMin) m
        (m'', mMax, _ ) = M.splitLookup (r ^. rMax) m'
    setRange r s = S.unions $ s''
                            : (S.singleton (r ^. rMin) <$ guard sMin)
                           ++ (S.singleton (r ^. rMax) <$ guard sMax)
      where
        (_  , sMin, s') = S.splitMember (r ^. rMin) s
        (s'', sMax, _ ) = S.splitMember (r ^. rMax) s'

-- | Turn a set of coordinates into a map of x's to the y's found in
-- the set.
--
-- Note that this forms an isomorphism with 'fromCoordMap'.
toCoordMap
    :: Eq a
    => S.Set (Coord a)
    -> M.Map a (S.Set a)
toCoordMap = fmap (S.fromDistinctAscList . ($ []))
           . M.fromAscListWith (.)
           . foldMap (\case C x y -> [(x, (y:))])

-- | Convert a map of x's to y's into a set of x-y coordinates.
--
-- Note that this forms an isomorphism with 'toCoordMap'.
fromCoordMap
    :: M.Map a (S.Set a)
    -> S.Set (Coord a)
fromCoordMap = S.fromDistinctAscList
             . M.foldMapWithKey (\k -> foldMap ((:[]) . C k))

-- | Render a single according to a 'PointStyle'.
renderPoint
    :: PointStyle
    -> Image
renderPoint PointStyle{..} = char (defAttr `withForeColor` _psColor) _psMarker
