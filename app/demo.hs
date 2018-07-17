import           Control.Monad
import           Graphics.Vty
import           Interactive.Plot
import           System.Exit

cosTest :: Series
cosTest = Series { sItems = map ((\i -> C i (cos i)) . (/ 20) . fromInteger) [-100..100]
                 , sStyle = PointStyle '*' blue
                 }

lineTest :: Series
lineTest = Series { sItems = map ((\i -> C i i) . (/ 4) . fromInteger) [-20..20]
                  , sStyle = PointStyle '+' red
                  }

main :: IO ()
main = do
    vty <- mkVty =<< standardIOConfig
    (wd,ht) <- displayBounds $ outputIface vty
    update vty . picForLayers $
      renderPlot (C (R 0    wd) (R 0    ht))
                 (PRX (R (-6) 6) (RR 0.5 2.1))
                 [cosTest, lineTest]

    forever $ do
      e <- nextEvent vty
      case e of
        EvKey KEsc [] -> shutdown vty *> exitSuccess
        _             -> pure ()

