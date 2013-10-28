import Graphics.HsPlot hiding (Colour)
import Graphics.HsPlot.Samples

main = do
  ds <- dsmall
  renderToPNG 500 300 "fig2.2-a.png" $
    plot ds (aes {x=carat, y=price, colour=color}) [pointLayer]
  renderToPNG 500 300 "fig2.2-b.png" $
    plot ds (aes {x=carat, y=price, shape=cut}) [pointLayer]
  di <- diamonds
  renderToPNG 300 300 "fig2.3-a.png" $
    plot di (aes {x=carat, y=price, alpha=const (0.1::Double)}) [pointLayer]
  renderToPNG 300 300 "fig2.3-b.png" $
    plot di (aes {x=carat, y=price, alpha=const (0.01::Double)}) [pointLayer]
  renderToPNG 300 300 "fig2.3-c.png" $
    plot di (aes {x=carat, y=price, alpha=const (0.005::Double)}) [pointLayer]
  de <- economics
  renderToPNG 500 300 "fig2.14-b.png" $
    plot de (aes {x=date, y=uempmed}) [lineLayer]
  dc <- mtcars
  renderToPNG 500 300 "fig3.1.png" $
    plot dc (aes {x=displ, y=hwy, colour=Factor . cyl}) [pointLayer]
