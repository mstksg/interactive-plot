{-# LANGUAGE LambdaCase #-}

import           Control.Concurrent
import           Control.Monad
import           Graphics.Vty
import           Interactive.Plot
import           System.Exit

testSeries :: Series
testSeries = Series { sItems = map ((\i -> C i (cos i)) . (/ 40) . fromInteger) [-200..200]
                    , sStyle = PointStyle '*' blue
                    }


main :: IO ()
main = do
    vty <- mkVty =<< standardIOConfig
    (wd,ht) <- displayBounds $ outputIface vty
    update vty . picForLayers $
      renderPlot (C (R 0    wd) (R 0    ht))
                 (C (R (-6) 6 ) (R (-2) 2 ))
                 [testSeries]

    forever $ do
      e <- nextEvent vty
      case e of
        EvKey KEsc [] -> shutdown vty *> exitSuccess
        _             -> pure ()

-- processEvt
--     :: Event -> Maybe SimEvt
-- processEvt = \case
--     EvKey KEsc        []      -> Just SEQuit
--     EvKey (KChar 'c') [MCtrl] -> Just SEQuit
--     EvKey (KChar 'q') []      -> Just SEQuit
--     EvKey (KChar '+') []      -> Just $ SEZoom (sqrt 2)
--     EvKey (KChar '-') []      -> Just $ SEZoom (sqrt 0.5)
--     EvKey (KChar '>') []      -> Just $ SERate (sqrt 2)
--     EvKey (KChar '<') []      -> Just $ SERate (sqrt (1/2))
--     EvKey (KChar ']') []      -> Just $ SEHist 5
--     EvKey (KChar '[') []      -> Just $ SEHist (-5)
--     _                         -> Nothing
--       -- forM_ (processEvt e) $ \case
--       --   SEQuit -> do
--       --     killThread t
--       --     shutdown vty
--       --     exitSuccess
--       --   SEZoom s ->
--       --     modifyIORef opts $ \o -> o { soZoom = soZoom o * s }
--       --   SERate r ->
--       --     modifyIORef opts $ \o -> o { soRate = soRate o * r }
--       --   SEHist h ->
--       --     modifyIORef opts $ \o -> o { soHist = soHist o + h }

