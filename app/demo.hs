import           Data.Default
import           Interactive.Plot

cosTest  = funcSeries cos (enumRange 100 (R (-5) 5)) def
lineTest = funcSeries id  (enumRange 20  (R (-4) 4)) def

main :: IO ()
main = runPlot def (Just (R (-6) 6)) Nothing $
            fromAutoSeries [cosTest, lineTest]

