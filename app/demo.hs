import           Interactive.Plot

cosTest, lineTest :: AutoSeries
cosTest  = funcSeries cos (enumRange 100 (R (-5) 5)) mempty
lineTest = funcSeries id  (enumRange 20  (R (-4) 4)) mempty

main :: IO ()
main = runPlotAuto defaultPlotOpts (Just "test") [cosTest, lineTest]

