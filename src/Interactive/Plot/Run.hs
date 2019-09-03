{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}

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
    runPlot
  , runPlotAuto
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.IORef
import           Graphics.Vty
import           Interactive.Plot.Core
import           Interactive.Plot.Series
import           Lens.Micro

data PEvent = PEQuit
            | PEZoom (Coord Double)
            | PEPan    (Coord Double)
            | PEResize (Coord Int)
            | PEReset

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
    EvKey KLeft       []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey KDown       []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey KUp         []      -> Just $ PEPan  (C 0      0.2   )
    EvKey KRight      []      -> Just $ PEPan  (C 0.2    0     )
    EvKey (KChar 'v') []      -> Just $ PEZoom (C 1          (sqrt 2  ))
    EvKey (KChar '^') []      -> Just $ PEZoom (C 1          (sqrt 0.5))
    EvKey (KChar '<') []      -> Just $ PEZoom (C (sqrt 2  ) 1         )
    EvKey (KChar '>') []      -> Just $ PEZoom (C (sqrt 0.5) 1         )
    EvResize ht wd            -> Just $ PEResize (C ht wd)
    _                         -> Nothing

data PlotState = PlotState
    { _psRange    :: Coord (Range Double)
    , _psSerieses :: [Series]
    }

psRange :: Lens' PlotState (Coord (Range Double))
psRange f (PlotState r s) = (`PlotState` s) <$> f r

displayRange :: Output -> IO (Coord (Range Int))
displayRange o = do
    (wd, ht) <- displayBounds o
    pure $ C (R 0 wd) (R 0 ht)

-- | Interactively plot auto-serieses in the terminal.
runPlotAuto
    :: PlotOpts
    -> [AutoSeries]
    -> IO ()
runPlotAuto po = runPlot po . fromAutoSeries

-- | Interactively plot serieses in the terminal.
runPlot
    :: PlotOpts
    -> [Series]
    -> IO ()
runPlot po ss = do
    vty   <- mkVty =<< standardIOConfig
    psRef <- newIORef =<< initPS vty
    peChan <- newChan
    tPE <- forkIO . forever $
      traverse_ (writeChan peChan) . processEvent =<< nextEvent vty

    void . runMaybeT . many . MaybeT . fmap guard $
      plotLoop vty peChan psRef tPE
  where
    initPS :: Vty -> IO PlotState
    initPS vty = do
      dr    <- displayRange $ outputIface vty
      pure PlotState { _psRange    = plotRange po dr ss
                     , _psSerieses = ss
                     }
    plotLoop
        :: Vty
        -> Chan PEvent
        -> IORef PlotState
        -> ThreadId
        -> IO Bool
    plotLoop vty peChan psRef tPE = do
      dr      <- displayRange $ outputIface vty
      ps      <- readIORef psRef
      let imgs = renderPlot dr (_psRange ps) (_psSerieses ps)

      update vty $ picForLayers imgs
      readChan peChan >>= \case
        PEQuit -> do
          killThread tPE
          shutdown vty
          pure False
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
        PEReset -> do
          writeIORef psRef =<< initPS vty
          pure True
