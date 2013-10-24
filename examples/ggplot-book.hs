import Graphics.HsPlot hiding (Colour)
import Graphics.HsPlot.Samples

main = do
  ds <- dsmall
  di <- diamonds
  renderToPNG 500 300 "fig2.2-a.png" $
    plot ds [Layer Point $ aes {x=carat, y=price, colour=color}]
  renderToPNG 500 300 "fig2.2-b.png" $
    plot ds [Layer Point $ aes {x=carat, y=price, shape=cut}]
  renderToPNG 300 300 "fig2.3-a.png" $
    plot di [Layer Point $ aes {x=carat, y=price, alpha=const (0.1::Double)}]
  renderToPNG 300 300 "fig2.3-b.png" $
    plot di [Layer Point $ aes {x=carat, y=price, alpha=const (0.01::Double)}]
  renderToPNG 300 300 "fig2.3-c.png" $
    plot di [Layer Point $ aes {x=carat, y=price, alpha=const (0.005::Double)}]
