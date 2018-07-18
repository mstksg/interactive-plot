import           Graphics.Vty
import           Interactive.Plot

cosTest :: Series
cosTest = Series { sItems = map ((\i -> C i (cos i)) . (/ 20) . fromInteger) [-100..100]
                 , sStyle = PointStyle '*' blue
                 }

lineTest :: Series
lineTest = Series { sItems = map ((\i -> C i i) . (/ 16) . fromInteger) [-80..80]
                  , sStyle = PointStyle '+' red
                  }

main :: IO ()
main = runPlot (PRX (R (-6) 6) (RR 0.5 2.1))
            [cosTest, lineTest]

