import           Interactive.Plot

cosTest, lineTest :: AutoSeries
cosTest  = funcSeries cos (enumRange 100 (R (-5) 5)) mempty
lineTest = funcSeries id  (enumRange 20  (R (-4) 4)) mempty

sinTest :: Double -> AutoSeries
sinTest t = funcSeries (sin . (+ t)) (enumRange 100 (R (-5) 5)) mempty

main :: IO ()
main = do
    runPlotAuto defaultPlotOpts (Just "simple test") [cosTest, lineTest]
    animatePlotFunc (defaultPlotOpts { _poFramerate = Just 20 }) (Just "animate test") $
      Just . fromAutoSeries . (:[]) . sinTest

