{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ViewPatterns              #-}

-- |
-- Module      : Interative.Plot.Run
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Run plots interactively in the terminal.
module Interactive.Plot.Run (
  -- * Simple
    runPlot
  , runPlotAuto
  -- * Animated
  , animatePlot, lastForever
  , animatePlotFunc
  , animatePlotMoore, Moore(..)
  -- * Custom
  , runPlotDynamic
  , PlotData(..), pdTitle, pdSerieses, pdDesc

  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Graphics.Vty hiding       ((<|>))
import           Interactive.Plot.Core
import           Interactive.Plot.Series
import           Lens.Micro
import           Lens.Micro.TH
import           Text.Printf
import qualified Data.List.NonEmpty        as NE

data PEvent = PEQuit
            | PEZoom (Coord Double)
            | PEPan    (Coord Double)
            | PEResize (Coord Int)
            | PEHelp
            | PEReset
            | PETick

processEvent :: Event -> Maybe PEvent
processEvent = \case
    EvKey KEsc        []      -> Just PEQuit
    EvKey (KChar 'c') [MCtrl] -> Just PEQuit
    EvKey (KChar 'q') []      -> Just PEQuit
    EvKey (KChar 'r') []      -> Just PEReset
    EvKey (KChar 'R') []      -> Just PEReset
    EvKey (KChar '=') []      -> Just $ PEZoom (C (sqrt 0.5) (sqrt 0.5))
    EvKey (KChar '+') []      -> Just $ PEZoom (C (sqrt 0.5) (sqrt 0.5))
    EvKey (KChar '-') []      -> Just $ PEZoom (C (sqrt 2  ) (sqrt 2  ))
    EvKey (KChar '_') []      -> Just $ PEZoom (C (sqrt 2  ) (sqrt 2  ))
    EvKey (KChar 'h') []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey (KChar 'j') []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey (KChar 'k') []      -> Just $ PEPan  (C 0      0.2   )
    EvKey (KChar 'l') []      -> Just $ PEPan  (C 0.2    0     )
    EvKey (KChar 'w') []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey (KChar 'a') []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey (KChar 's') []      -> Just $ PEPan  (C 0      0.2   )
    EvKey (KChar 'd') []      -> Just $ PEPan  (C 0.2    0     )
    EvKey KLeft       []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey KDown       []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey KUp         []      -> Just $ PEPan  (C 0      0.2   )
    EvKey KRight      []      -> Just $ PEPan  (C 0.2    0     )
    EvKey (KChar 'v') []      -> Just $ PEZoom (C 1          (sqrt 2  ))
    EvKey (KChar '^') []      -> Just $ PEZoom (C 1          (sqrt 0.5))
    EvKey (KChar '<') []      -> Just $ PEZoom (C (sqrt 2  ) 1         )
    EvKey (KChar '>') []      -> Just $ PEZoom (C (sqrt 0.5) 1         )
    EvKey (KChar '?') []      -> Just PEHelp
    EvKey (KChar '/') []      -> Just PEHelp
    EvResize ht wd            -> Just $ PEResize (C ht wd)
    _                         -> Nothing

data PlotState = PlotState
    { _psRange    :: Coord (Range Double)
    , _psHelp     :: Bool
    }

makeClassy ''PlotState

displayRange :: Output -> IO (Coord (Range Int))
displayRange o = do
    (wd, ht) <- displayBounds o
    pure $ C (R 0 wd) (R 0 ht)

-- | Dynamically adjustable plot data.
data PlotData = PlotData
    { _pdTitle    :: Maybe String
    , _pdDesc     :: Maybe Image
    , _pdSerieses :: [Series]
    }

-- | Getter/setter lens to the title field of a 'PlotData'
pdTitle :: Lens' PlotData (Maybe String)
pdTitle f (PlotData x y z) = (\x' -> PlotData x' y z) <$> f x

-- | Getter/setter lens to the description box field of a 'PlotData'
pdDesc :: Lens' PlotData (Maybe Image)
pdDesc f (PlotData x y z) = (\y' -> PlotData x y' z) <$> f y

-- | Getter/setter lens to the serieses field of a 'PlotData'
pdSerieses :: Lens' PlotData [Series]
pdSerieses f (PlotData x y z) = PlotData x y <$> f z

-- | Display fixed plot and title interactively, filling in default values.
--
-- See 'runPlotDynamic' for more control.
runPlotAuto
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts')
    -> Maybe String     -- ^ title
    -> [AutoSeries]     -- ^ uninitialized series data
    -> IO ()
runPlotAuto po t s = case po ^. poAutoMethod of
    Nothing -> runPlot po t =<< fromAutoSeriesIO s
    Just g  -> runPlot po t $ fromAutoSeries_ g s

-- | Display fixed plot and title interactively.
--
-- See 'runPlotDynamic' for more control.
runPlot
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts')
    -> Maybe String     -- ^ title
    -> [Series]         -- ^ series data
    -> IO ()
runPlot po t s = runPlotDynamic po
    (const (pure True))
    (pure (Just (PlotData t (_poDescription po) s)))

-- | Display a series of plots (@['Series']@) with a time delay between
-- each one.  Will quit when the last plot is displayed.  Use 'lastForever'
-- on the input list to repeat the last item indefinitely, or 'cycle' to
-- cycle through the list forever.
--
-- Note that this behavior is pretty simple; more advanced functionality
-- can be achieved with 'runPlotDynamic' directly.
animatePlot
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts')
    -> Double           -- ^ update rate (frames per second)
    -> Maybe String     -- ^ title
    -> [[Series]]       -- ^ list of series data (potentially infinite)
    -> IO ()
animatePlot po fps t ss = do
    ssRef    <- newEmptyMVar
    rateMult <- newIORef 0
    tid      <- forkIO $ do
      forM_ ss $ \s -> do
        putMVar ssRef (Just s)
        threadDelay . mkDelay =<< readIORef rateMult
        takeMVar ssRef
      putMVar ssRef Nothing
    runPlotDynamic po' (updateFr rateMult) (mkData rateMult ssRef)
    killThread tid
  where
    mkDelay i = round $ 1000000 / (fps * (2 ** (fromIntegral i / 2)))
    mkData rateMult ssRef = do
      ss' <- readMVar ssRef
      desc <- animateDesc (_poDescription po) <$> readIORef rateMult
      pure $ PlotData t desc <$> ss'
    po' = po & poFramerate %~ (<|> Just (max fps 10))
    updateFr :: IORef Int -> Event -> IO Bool
    updateFr rateMult = \case
      EvKey (KChar '[') []      -> True <$ modifyIORef rateMult (subtract 1)
      EvKey (KChar ']') []      -> True <$ modifyIORef rateMult (+ 1)
      _                         -> pure True

-- | Handy function to use with 'animatePlot' to extend the last frame into
-- eternity.
lastForever :: [a] -> [a]
lastForever []           = []
lastForever [x]          = repeat x
lastForever (x:xs@(_:_)) = x : lastForever xs

animateDesc :: Maybe Image -> Int -> Maybe Image
animateDesc d r = desc' <|> Just desc
  where
    desc  = string defAttr $ "[/]    rate" ++ rString
    desc' = (`vertJoin` desc) . (`vertJoin` char defAttr ' ') <$> d
    rString
      | r == 0    = ""
      | otherwise = printf " (x%.2f)" $ 2 ** (fromIntegral @_ @Double r / 2)


-- | Animate (according to the framerate in the 'PlotOpts') a function
-- @'Double' -> 'Maybe' [Series]@, where the input is the current time in
-- seconds and the output is the plot to display at that time.  Will quit
-- as soon as 'Nothing' is given.
--
-- Remember to give a 'PlotOpts' with a 'Just' framerate.
--
-- This is a simple wrapper over 'animatePlotMoore' with a stateless
-- function.  For more advanced functionality, use 'animatePlotMoore' or
-- 'runPlotDynamic' directly.
animatePlotFunc
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts', but remember to set a framerate)
    -> Maybe String                 -- ^ title
    -> (Double -> Maybe [Series])   -- ^ function from time to plot. will quit as soon as 'Nothing' is returned.
    -> IO ()
animatePlotFunc po t f = animatePlotMoore po t $ Moore
    { moInitVal   = f 0
    , moInitState = 0
    , moUpdate    = \dt tt ->
        let t' = tt + dt
        in  pure $ (, t') <$> f t'
    }

-- | Used for 'animatePlotMoore' to specify how a plot evolves over time
-- with some initial state.
data Moore a = forall s. Moore
    { -- | initial value of plot.  'Nothing' for a non-starter.
      moInitVal   :: Maybe a
      -- | initial state of plot
    , moInitState :: s
      -- | Given change in time since last render and old state, return new
      -- plot and state. Return 'Nothing' to quit.
    , moUpdate    :: Double -> s -> IO (Maybe (a, s))
    }

deriving instance Functor Moore

-- | Animate (according to the framerate in the 'PlotOpts') a "Moore
-- machine" description of a plot evolving over time with some initial
-- state.
--
-- Remember to give a 'PlotOpts' with a 'Just' framerate.
--
-- For a simplified version of a stateless function, see 'animatePlotFunc'.
-- This is implemented in terms of 'runPlotDynamic', but the representation
-- of an animation in terms of a moore machine is powerful enough to
-- represent a very general class of animations.
animatePlotMoore
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts', but remember to set a framerate)
    -> Maybe String     -- ^ title
    -> Moore [Series]   -- ^ moore machine representing progression of plot from an initial state
    -> IO ()
animatePlotMoore po t Moore{..} = do
    ssRef     <- newIORef moInitVal
    rateMult  <- newIORef 0
    currState <- newIORef moInitState
    tid   <- forkIO . void . runMaybeT . many . MaybeT . fmap guard $ do
      threadDelay td
      dt <- mkDT <$> readIORef rateMult
      s  <- readIORef currState
      moUpdate dt s >>= \case
        Nothing       -> False <$ writeIORef ssRef Nothing
        Just (xs, s') -> True <$ do
          writeIORef ssRef (Just xs)
          writeIORef currState s'
    runPlotDynamic po (updateFr rateMult) (mkData rateMult ssRef)
    killThread tid
  where
    fps = fromMaybe 1       $ po ^. poFramerate
    td  = fromMaybe 1000000 $ po ^. poDelay
    mkDT i = 1 / (fps * (2 ** (- fromIntegral i / 2)))
    mkData rateMult ssRef = do
      ss' <- readIORef ssRef
      desc <- animateDesc (_poDescription po) <$> readIORef rateMult
      pure $ PlotData t desc <$> ss'
    updateFr :: IORef Int -> Event -> IO Bool
    updateFr rateMult = \case
      EvKey (KChar '[') []      -> True <$ modifyIORef rateMult (subtract 1)
      EvKey (KChar ']') []      -> True <$ modifyIORef rateMult (+ 1)
      _                         -> pure True


-- | Version of 'runPlot' that allows you to vary the plotted data and the
-- title.  It will execute the @'IO' PlotData@ to get the current plot
-- data; you can use this with i.e. an 'IORef' to adjust the data in
-- real-time.
runPlotDynamic
    :: PlotOpts
    -> (Event -> IO Bool)   -- ^ process VTY events (return 'False' to trigger quit)
    -> IO (Maybe PlotData)  -- ^ action to "get" the plot data every frame. if 'Nothing', quit.
    -> IO ()
runPlotDynamic po pe ssRef = do
    vty     <- mkVty =<< standardIOConfig
    pdmaybe <- ssRef
    forM_ pdmaybe $ \initPD -> do
      psRef  <- newIORef =<< initPS vty initPD
      peChan <- newChan
      tPE <- forkIO . forever $ do
        e <- nextEvent vty
        q <- pe e
        unless q $ writeChan peChan PEQuit
        traverse_ (writeChan peChan) $ processEvent e
      tTick <- forM (po ^. poDelay) $ \td ->
        forkIO . forever $ do
          threadDelay td
          writeChan peChan PETick

      void . runMaybeT . many . MaybeT . fmap guard $
        plotLoop vty peChan psRef
      killThread tPE
      traverse_ killThread tTick
      shutdown vty
  where
    initPS :: Vty -> PlotData -> IO PlotState
    initPS vty PlotData{..} = do
      dr    <- displayRange $ outputIface vty
      pure $ PlotState
        { _psRange    = plotRange po dr _pdSerieses
        , _psHelp     = po ^. poHelp
        }
    plotLoop
        :: Vty
        -> Chan PEvent
        -> IORef PlotState
        -> IO Bool
    plotLoop vty peChan psRef = do
      dr           <- displayRange $ outputIface vty
      ps           <- readIORef psRef
      pdmaybe      <- ssRef
      fmap or . forM pdmaybe $ \pd@PlotData{..} -> do
        let titleBox = fmap (vertCat . intersperse (char defAttr ' ') . toList) . NE.nonEmpty . catMaybes $
                [ string (withStyle defAttr bold) <$> _pdTitle
                , _pdDesc
                ]
            uiText = case (titleBox, _psHelp ps) of
              (Nothing, False) -> id
              (Just t , False) -> (box t ++)
              (Nothing, True ) -> (box helpBox ++)
              (Just t , True ) -> (box (vertCat [t, char defAttr ' ', helpBox]) ++)
            imgs = uiText $ renderPlot dr (_psRange ps) _pdSerieses

        update vty $ picForLayers imgs
        hideCursor . outputIface $ vty
        readChan peChan >>= \case
          PEQuit -> pure False
          PEZoom d -> do
            let scaler s = over rSize (* s)
            writeIORef psRef $
              ps & psRange %~ (<*>) (scaler <$> d)
            pure True
          PEPan d -> do
            let panner s r = fmap (+ (r ^. rSize * s)) r
            writeIORef psRef $
              ps & psRange %~ (<*>) (panner <$> d)
            pure True
          PEResize newDim -> do
            let oldDim = _rSize <$> dr
                newRange = do
                  d0 <- oldDim
                  d1 <- newDim
                  r0 <- _psRange ps
                  pure $ r0 & rSize %~ (* (fromIntegral d1 / fromIntegral d0))
            writeIORef psRef $
              ps & psRange .~ newRange
            pure True
          PEHelp -> do
            writeIORef psRef $
              ps & psHelp %~ not
            pure True
          PEReset -> do
            writeIORef psRef =<< initPS vty pd
            pure True
          PETick  -> pure True

helpText :: [(String, String)]
helpText =
    [ ("-/+"   , "zoom")
    , ("arrows", "pan")
    , ("v/^"   , "vert stretch")
    , ("</>"   , "horiz stretch")
    , ("r"     , "reset disp")
    , ("?"     , "show help")
    , ("q"     , "quit")
    ]

helpBox :: Image
helpBox = vertCat (string defAttr . (++ " ") <$> x)
    `horizJoin` vertCat (string defAttr <$> y)
  where
    (x,y) = unzip helpText

box :: Image -> [Image]
box (pad 1 0 1 0 -> i) = [boxed, charFill defAttr ' ' (imageWidth i + 1) (imageHeight i + 1)]
  where
    lr = charFill defAttr '|' 1 (imageHeight i)
    tb = charFill defAttr '-' (imageWidth i) 1
    c  = char defAttr '+'
    boxed = vertCat . map horizCat $
      [ [c , tb, c ]
      , [lr, i , lr]
      , [c , tb, c ]
      ]
