{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

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
  , runPlotDynamic
  , PlotData(..), pdTitle, pdSerieses
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
import           Lens.Micro.TH

data PEvent = PEQuit
            | PEZoom (Coord Double)
            | PEPan    (Coord Double)
            | PEResize (Coord Int)
            | PEHelp
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
    EvKey (KChar '?') []      -> Just $ PEHelp
    EvKey (KChar '/') []      -> Just $ PEHelp
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

data PlotData = PlotData
    { _pdTitle    :: Maybe String
    , _pdSerieses :: [Series]
    }

makeLenses ''PlotData

runPlotAuto
    :: PlotOpts
    -> Maybe String     -- ^ title
    -> [AutoSeries]     -- ^ uninitialized series data
    -> IO ()
runPlotAuto po t s = case po ^. poAutoMethod of
    Nothing -> runPlot po t =<< fromAutoSeriesIO s
    Just g  -> runPlot po t $ fromAutoSeries_ g s

runPlot
    :: PlotOpts
    -> Maybe String     -- ^ title
    -> [Series]         -- ^ series data
    -> IO ()
runPlot po t s = runPlotDynamic po . readIORef =<< newIORef (PlotData t s)

-- | Interactively plot serieses in the terminal.
runPlotDynamic
    :: PlotOpts
    -> IO PlotData
    -> IO ()
runPlotDynamic po ssRef = do
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
      PlotData{..} <- ssRef
      pure PlotState { _psRange    = plotRange po dr _pdSerieses
                     , _psHelp     = po ^. poHelp
                     }
    plotLoop
        :: Vty
        -> Chan PEvent
        -> IORef PlotState
        -> ThreadId
        -> IO Bool
    plotLoop vty peChan psRef tPE = do
      dr           <- displayRange $ outputIface vty
      ps           <- readIORef psRef
      PlotData{..} <- ssRef
      let uiText = case (string (withStyle defAttr bold) <$> _pdTitle, _psHelp ps) of
            (Nothing, False) -> id
            (Just t , False) -> (box t ++)
            (Nothing, True ) -> (box helpBox ++)
            (Just t , True ) -> (box (vertCat [t, char defAttr ' ', helpBox]) ++)
          imgs = uiText $ renderPlot dr (_psRange ps) _pdSerieses

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
        PEHelp -> do
          writeIORef psRef $
            ps & psHelp %~ not
          pure True
        PEReset -> do
          writeIORef psRef =<< initPS vty
          pure True

helpText :: [(String, String)]
helpText =
    [ ("-/+"   , "zoom")
    , ("arrows", "pan")
    , ("v/^"   , "vert stretch")
    , ("</>"   , "horiz stretch")
    , ("r"     , "reset")
    , ("?"     , "disp help")
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
