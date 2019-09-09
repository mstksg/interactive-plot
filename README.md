interactive-plot
================

Quick time series terminal plotting for data exploration/in ghci.

Most commonly used imports should be available in *Interactive.Plot*.

Construct a `Series` from scratch using the raw data type, or use one of the
handy helpers:

1.  `listSeries`: Create a series from a list or any foldable.
2.  `tupleSeries`: Create a series from a list of ordered-pair tuples providing
    x and y locations.
3.  `funcSeries`: Create a series from a function `Double -> Double`, given a
    range of `x`s to produce the `y`s.

Then simply "run" a list of series (or "automatic-styled series") using
`runPlotAuto` or `runPlot`:

```haskell
runPlotAuto
    :: PlotOpts         -- ^ options (can be 'defaultPlotOpts')
    -> Maybe String     -- ^ optional title
    -> [AutoSeries]     -- ^ uninitialized data of serieses
    -> IO ()
```

![Static Plots](https://i.imgur.com/SkH1C1j.gif)

These plots can be zoomed, stretched, scaled, panned interactively after
launch.  If you quit, things resume back to the ghci session (or whatever point
in the program you launch from).

There are also options for rudimentary animations:

```haskell
animatePlot
    :: PlotOpts
    -> Double           -- ^ update rate (frames per second)
    -> Maybe String     -- ^ title
    -> [[Series]]       -- ^ list of series data (potentially infinite)
    -> IO ()

animatePlotFunc
    :: PlotOpts
    -> Maybe String                 -- ^ title
    -> (Double -> Maybe [Series])   -- ^ function from time to plot. will quit as soon as 'Nothing' is returned.
    -> IO ()
```

![Animated Plots](https://i.imgur.com/37Cq8ET.gif)

Todo
----

*   Consider being able to use functions directly as a series, instead of
    converting them into ordered pairs based on a known x series.
