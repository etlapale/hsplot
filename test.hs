import Graphics.HsPlot
import Graphics.HsPlot.Samples

main = do
  dset <- mtcars
  renderToPNG 600 400 "hsplot.png" $
    plot dset [Layer Point $ aes {x=displ, y=hwy, colour=cyl}]
