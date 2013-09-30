import Graphics.HsPlot


data Diamond = Diamond { carat :: Double
                       , cut :: Cut
                       , colour :: Colour
                       , clarity :: Clarity
                       , depth :: Double
                       , table :: Double
                       , price :: Int
                       , x' :: Double
                       , y' :: Double
                       , z' :: Double
                       }

data Cut = Fair | Good | VeryGood | Premium | Ideal
data Colour = D | E | F | G | H | I | J
data Clarity = I3 | I2 | I1 | SI2 | SI1 | VS2 | VS1 | VVS2 | VVS1 | IF | FL

diamonds = [ Diamond 0.2 Ideal E SI2 61.5 55.0 326 3.95 3.98 2.43
           , Diamond 0.2 Premium E SI1 59.8 61.0 326 3.89 3.84 2.31
           , Diamond 0.2 Good E VS1 56.9 65.0 327 4.05 4.07 2.31
           , Diamond 0.3 Premium I VS2 62.4 58.0 334 4.20 4.23 2.63
           , Diamond 0.3 Good J SI2 63.3 58.0 335 4.34 4.35 2.75
           , Diamond 0.2 VeryGood J VVS2 62.8 57.0 336 3.94 3.96 2.48
           ]

main = renderToPNG "hsplot.png" $ plot diamonds layers

layers = [Layer Point $ aes {x=carat, y=(realToFrac . price)}]
