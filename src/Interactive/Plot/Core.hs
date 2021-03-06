{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
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
  , Range(.., RAbout), _rMid, _rSize', rMin, rMax, rSize, rMid, _rSize
  , Auto(..)
  , PointStyle, pattern PointStyle, _psMarker, _psColor, PointStyleF(..), AutoPointStyle, psMarker, psColor
  , Series, SeriesF(..), AutoSeries, sItems, sStyle, toCoordMap, fromCoordMap
  , Alignment(..)
  , PlotOpts(..), poTermRatio, poAspectRatio, poXRange, poYRange, poRange, poAutoMethod, poHelp, poFramerate, poDelay, poDescription
  , defaultPlotOpts
  , renderPlot
  -- * Internal
  , plotRange
  , OrdColor(..)
  , renderPoint
  , hzToDelay
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Coerce
import           Data.Default.Class
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
data Coord a = C { _cX :: a   -- ^ Access @x@.
                 , _cY :: a   -- ^ Access @y@.
                 }
  deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

-- | Getter/setter lens to the @x@ position in a 'Coord'.
cX :: Lens' (Coord a) a
cX f (C x y) = (`C` y) <$> f x

-- | Getter/setter lens to the @x@ position in a 'Coord'.
cY :: Lens' (Coord a) a
cY f (C x y) = C x <$> f y

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

-- | Basically the same as @Reader 'Bool'@, for @x@ and @y@.
instance Monad Coord where
    return x = C x x
    C x y >>= f = C (_cX (f x)) (_cY (f y))

-- | A specification for a range.  Using 'R', contains the minimum and
-- maximum.  Using 'RAbout', contains the midpoint and size.
data Range a = R { _rMin :: a     -- ^ Minimum of range.
                 , _rMax :: a     -- ^ Maximum of range.
                 }
  deriving (Show, Functor, Foldable, Traversable)


-- | Getter/setter lens to the minimum value in a 'Range'.
rMin :: Lens' (Range a) a
rMin f (R x y) = (`R` y) <$> f x

-- | Getter/setter lens to the maximum value in a 'Range'.
rMax :: Lens' (Range a) a
rMax f (R x y) = R x <$> f y

-- | "Zipping" behavior on minimum and maximum
instance Applicative Range where
    pure x = R x x
    R f g <*> R x y = R (f x) (g y)

-- | Basically the same as @Reader 'Bool'@, for minimum and maximum
-- fields.
instance Monad Range where
    return x = R x x
    R x y >>= f = R (_rMin (f x)) (_rMax (f y))

-- | Used to specify fields in 'PointStyle' and 'Series': Use 'Auto' for
-- automatic inference, and 'Given' to provide a specific value.
--
-- Its 'Semigroup' instance keeps the last 'Given'.
data Auto a = Auto | Given a
  deriving (Show, Eq, Ord, Generic, Functor)

-- | Keeps the final 'Given': like 'Data.Monoid.Last'
instance Semigroup (Auto a) where
    (<>) = \case
      Auto    -> id
      Given x -> \case Auto -> Given x; Given y -> Given y

instance Monoid (Auto a) where
    mempty = Auto
    mappend = (<>)

instance Applicative Auto where
    pure = Given
    (<*>) = \case
      Auto    -> const Auto
      Given f -> \case
        Auto    -> Auto
        Given x -> Given (f x)

instance Monad Auto where
    return = Given
    (>>=) = \case
      Auto    -> const Auto
      Given x -> ($ x)

-- | Opposite behavior of 'Semigroup' instance: like 'Maybe's 'Alternative'
-- instance, or 'Data.Monoid.First'.
instance Alternative Auto where
    empty = Auto
    (<|>) = \case
      Auto    -> id
      Given x -> const (Given x)

-- | Opposite behavior of 'Semigroup' instance: like 'Maybe's 'Alternative'
-- instance, or 'Data.Monoid.First'.
instance MonadPlus Auto

-- | A parameterized version of 'PointStyle' to unify functions in
-- "Interactive.Plot.Series".
--
-- Mainly you will be using either 'PointStyle' or 'AutoPointStyle'.
data PointStyleF f = PointStyleF
      { _psMarkerF :: f Char  -- ^ Marker character.
      , _psColorF  :: f Color -- ^ Marker color.
      }
  deriving (Generic)

-- | Getter/setter lens to the marker field of a 'PointStyleF'
psMarkerF :: Lens' (PointStyleF f) (f Char)
psMarkerF f (PointStyleF x y) = (`PointStyleF` y) <$> f x

-- | Getter/setter lens to the color field of a 'PointStyleF'
psColorF :: Lens' (PointStyleF f) (f Color)
psColorF f (PointStyleF x y) = PointStyleF x <$> f y

-- | Specification of a style for a point.
--
-- Construct this wiht the 'PointStyle' pattern synonym.
type PointStyle     = PointStyleF Identity

-- | A version of 'PointStyle' where you can leave the marker or color
-- blank, to be automatically inferred.
--
-- You can construct this with the 'PointStyleF' constructor.
--
-- It has a very convenient 'Monoid' instance: 'mempty' gives
-- a 'PointStyle' where every field is 'Auto', and '<>' combines
-- 'PointStyle's field-by-field, keeping the last 'Given'.
type AutoPointStyle = PointStyleF Auto

-- | Pattern synonym/constructor for 'PointStyle'.
--
-- This comes with two record fields, '_psMarker' and '_psColor'.
pattern PointStyle :: Char -> Color -> PointStyle
pattern PointStyle { _psMarker, _psColor } = PointStyleF (Identity _psMarker) (Identity _psColor)
{-# COMPLETE PointStyle #-}

instance (Semigroup (f Char), Semigroup (f Color)) => Semigroup (PointStyleF f) where
    PointStyleF m1 c1 <> PointStyleF m2 c2 = PointStyleF (m1 <> m2) (c1 <> c2)
instance (Monoid (f Char), Monoid (f Color)) => Monoid (PointStyleF f) where
    mempty = PointStyleF mempty mempty

deriving instance (Show (f Char), Show (f Color)) => Show (PointStyleF f)
deriving instance (Eq (f Char), Eq (f Color)) => Eq (PointStyleF f)
instance (Ord (f Char), Ord (f OrdColor), Functor f, Eq (f Color)) => Ord (PointStyleF f) where
    compare = comparing $ \case PointStyleF m1 c1 -> (m1, OC <$> c1)

_Identity :: Lens (Identity a) (Identity b) a b
_Identity f (Identity x) = Identity <$> f x

psMarker :: Lens' PointStyle Char
psMarker = psMarkerF . _Identity

psColor :: Lens' PointStyle Color
psColor = psColorF . _Identity

-- | A parameterized version of 'Series' to unify functions in
-- "Interactive.Plot.Series".
--
-- Mainly you will be using either 'Series' or 'AutoSeries'.
data SeriesF f = Series { _sItems :: M.Map Double (S.Set Double)    -- ^ A map of @x@ positions to @y@ points at that position
                        , _sStyle :: PointStyleF f                  -- ^ The style of points.  For 'Series', this is 'PointStyle'; for 'AutoSeries', this is 'AutoPointStyle'.
                        }

deriving instance (Show (f Char), Show (f Color)) => Show (SeriesF f)

-- | Data for a single series: contains the coordinate map with the point
-- style for the series.
type Series = SeriesF Identity

-- | A version of 'Series' where you can leave the marker or color blank,
-- to be automatically inferred.
type AutoSeries = SeriesF Auto

-- | Getter/setter lens to the items field of a 'SeriesF'
sItems :: Lens' (SeriesF f) (M.Map Double (S.Set Double))
sItems f (Series x y) = (`Series` y) <$> f x

-- | Getter/setter lens to the style field of a 'SeriesF'
--
-- @
-- 'sStyle' :: Lens 'Series' 'PointStyle'
-- 'sStyle' :: Lens 'AutoSeries' 'AutoPointStyle'
-- @
sStyle :: Lens' (SeriesF f) (PointStyleF f)
sStyle f (Series x y) = Series x <$> f y

-- | Alignment specification.
data Alignment = ALeft
               | ACenter
               | ARight

-- | Options used for running the plot interactively in a terminal.
data PlotOpts = PO
    { _poTermRatio   :: Double               -- ^ character width ratio of terminal (H/W).  Default is 2.1.
    , _poAspectRatio :: Maybe Double         -- ^ plot aspect ratio (H/W). Use 'Nothing' for automatic. Default is @'Just' 1@.
    , _poXRange      :: Maybe (Range Double) -- ^ X Range.  Use 'Nothing' for automatic.  Default is 'Nothing'.
    , _poYRange      :: Maybe (Range Double) -- ^ Y Range.  Use 'Nothing' for automatic.  Default is 'Nothing'.
    , _poAutoMethod  :: Maybe StdGen         -- ^ How to fill in missing
                                             -- values when run using
                                             -- 'Interactive.Plot.Run.runPlotAuto'.
                                             -- 'Nothing' for IO, 'Just'
                                             -- for deterministic seed.
                                             -- Ignored when using
                                             -- 'Interactive.Plot.Run.runPlot'.
                                             -- Default is an arbitrarily
                                             -- selected seed.
    , _poHelp        :: Bool                 -- ^ Whether or not to show help box initially.  Box can always be toggled with @?@. (Default is 'True')
    , _poFramerate   :: Maybe Double         -- ^ Updates per second; 'Nothing' for no updates. Use 'poDelay' to treat this as a microsecond delay instead. (default: 'Nothing')
    , _poDescription :: Maybe Image          -- ^ Initial extra information in description box.  Ignored if directly using 'runPlotDynamic'.  Default is 'Nothing'
    }

makeLenses ''PlotOpts

-- | Sensible defaults for most terminals.
defaultPlotOpts :: PlotOpts
defaultPlotOpts = PO
    { _poTermRatio   = 2.1
    , _poAspectRatio = Just 1
    , _poXRange      = Nothing
    , _poYRange      = Nothing
    , _poAutoMethod  = Just $ mkStdGen 28922710942259
    , _poHelp        = True
    , _poFramerate   = Nothing
    , _poDescription = Nothing
    }

instance Default PlotOpts where
    def = defaultPlotOpts

-- | An alternative "constructor" for 'R', which takes a midpoint and size
-- instead of a min and max.
--
-- This comes with record fields, '_rMid' and '_rSize''.
pattern RAbout :: Fractional a => a -> a -> Range a
pattern RAbout { _rMid, _rSize' } <- (\case R{..} -> ((_rMin + _rMax) / 2, _rMax - _rMin)->(_rMid, _rSize'))
  where
    RAbout rM rS = R (rM - rS2) (rM + rS2)
      where
        rS2 = rS / 2
{-# COMPLETE RAbout #-}

-- | Gets the size of a 'Range'.
--
-- A version of '_rSize'' that works for any instance of 'Num'.
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
poRange f (PO r a x y s h t d) = (\(x', y') -> PO r a x' y' s h t d) <$> f (x, y)

-- | Lens into microsecond delay between frames, specified by a 'PlotOpts'.
poDelay :: Lens' PlotOpts (Maybe Int)
poDelay = poFramerate . hzToDelay

-- | Used for 'poDelay': a lens into a microsecond delay given
-- a framerate.  Should technically be an isomorphism, but this isn't
-- supported by microlens.
hzToDelay :: Lens' (Maybe Double) (Maybe Int)
hzToDelay f md = fmap back <$> f (fmap forward md)
  where
    back d    = 1000000 / fromIntegral d
    forward p = round $ 1000000 / p

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
