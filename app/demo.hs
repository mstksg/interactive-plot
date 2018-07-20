import           Data.Default
import           Interactive.Plot

cosTest, lineTest :: AutoSeries
cosTest  = funcSeries cos (enumRange 100 (R (-5) 5)) def
lineTest = funcSeries id  (enumRange 20  (R (-4) 4)) def

main :: IO ()
main = runPlotAuto def [cosTest, lineTest]

