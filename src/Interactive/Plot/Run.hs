{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns  #-}

module Interactive.Plot.Run (
    runPlot
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.IORef
import           Graphics.Vty
import           Interactive.Plot.Core

data PEvent = PEQuit
            | PEZoom   Double
            | PEPan    (Coord Double)
            | PEResize (Coord Int)

processEvent :: Event -> Maybe PEvent
processEvent = \case
    EvKey KEsc        []      -> Just PEQuit
    EvKey (KChar 'c') [MCtrl] -> Just PEQuit
    EvKey (KChar 'q') []      -> Just PEQuit
    EvKey (KChar '=') []      -> Just $ PEZoom (sqrt 0.5)
    EvKey (KChar '+') []      -> Just $ PEZoom (sqrt 0.5)
    EvKey (KChar '-') []      -> Just $ PEZoom (sqrt 2)
    EvKey (KChar '_') []      -> Just $ PEZoom (sqrt 2)
    EvKey (KChar 'h') []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey (KChar 'j') []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey (KChar 'k') []      -> Just $ PEPan  (C 0      0.2   )
    EvKey (KChar 'l') []      -> Just $ PEPan  (C 0.2    0     )
    EvKey KLeft       []      -> Just $ PEPan  (C (-0.2) 0     )
    EvKey KDown       []      -> Just $ PEPan  (C 0      (-0.2))
    EvKey KUp         []      -> Just $ PEPan  (C 0      0.2   )
    EvKey KRight      []      -> Just $ PEPan  (C 0.2    0     )
    EvResize ht wd            -> Just $ PEResize (C ht wd)
    _                         -> Nothing

data PlotState = PlotState
    { psRange    :: Coord (Range Double)
    , psSerieses :: [Series]
    }

displayRange :: Output -> IO (Coord (Range Int))
displayRange o = do
    (wd, ht) <- displayBounds o
    pure $ C (R 0 wd) (R 0 ht)

runPlot
    :: PlotRange
    -> [Series]
    -> IO ()
runPlot pr ss = do
    vty   <- mkVty =<< standardIOConfig
    dr    <- displayRange $ outputIface vty
    psRef <- newIORef PlotState { psRange    = plotRange dr pr
                                , psSerieses = ss
                                }

    peChan <- newChan
    tPE <- forkIO . forever $
      traverse_ (writeChan peChan) . processEvent =<< nextEvent vty

    void . runMaybeT . many . (guard =<<) . liftIO $
      plotLoop vty peChan psRef tPE
  where
    plotLoop
        :: Vty
        -> Chan PEvent
        -> IORef PlotState
        -> ThreadId
        -> IO Bool
    plotLoop vty peChan psRef tPE = do
      dr      <- displayRange $ outputIface vty
      ps      <- readIORef psRef
      let imgs = renderPlot dr (psRange ps) (psSerieses ps)

      update vty $ picForLayers imgs
      readChan peChan >>= \case
        PEQuit -> do
          killThread tPE
          shutdown vty
          pure False
        PEZoom d -> do
          writeIORef psRef $
            ps { psRange = scaleRange d <$> psRange ps }
          pure True
        PEPan d -> do
          writeIORef psRef $
            ps { psRange = (\s r -> fmap (+ (rSize r * s)) r) <$> d <*> psRange ps }
          pure True
        PEResize newDim -> do
          let oldDim = rSize <$> dr
              newRange = do
                d0 <- oldDim
                d1 <- newDim
                r0 <- psRange ps
                pure $ scaleRange (fromIntegral d1 / fromIntegral d0) r0
          writeIORef psRef $
            ps { psRange = newRange }
          pure True
