import Graphics.HsPlot

main = renderToPNG "hsplot.png" myplot
  where myplot = plot points [Layer Point tupleAes]
        points = [(1,9), (2,3), (4,1), (5,7), (6,3)]
