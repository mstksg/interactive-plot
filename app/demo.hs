import           Interactive.Plot

cosTest, lineTest :: AutoSeries
cosTest  = funcSeries cos (enumRange 100 (R (-5) 5)) mempty
lineTest = funcSeries id  (enumRange 20  (R (-4) 4)) mempty

sinTest :: Double -> AutoSeries
sinTest t = funcSeries (sin . (+ t)) (enumRange 100 (R (-5) 5)) mempty

main :: IO ()
main = do
    runPlotAuto defaultPlotOpts (Just "simple test") Nothing [cosTest, lineTest]
    animatePlot defaultPlotOpts 10 (Just "animate test") Nothing $
      map (fromAutoSeries . (:[]) . sinTest) [0,0.1..]
    animatePlotFunc (defaultPlotOpts { _poFramerate = Just 20 }) (Just "animate test") Nothing $
      Just . fromAutoSeries . (:[]) . sinTest

