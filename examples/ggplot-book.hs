import Graphics.HsPlot hiding (Colour)
import Graphics.HsPlot.Samples

main = do
  dset <- dsmall
  renderToPNG 500 300 "fig2.2-a.png" $
    plot dset [Layer Point $ aes {x=carat, y=price, colour=color}]
  renderToPNG 500 300 "fig2.2-b.png" $
    plot dset [Layer Point $ aes {x=carat, y=price, shape=cut}]
