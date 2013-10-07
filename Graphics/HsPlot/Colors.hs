module Graphics.HsPlot.Colors (
  lch2luv, lch2rgb, luv2xyz, xyz2rgb,
  uncurry3
) where

-- |Convert a CIE XYZ colour to sRGB.
xyz2rgb :: (Ord a, Floating a) => a -> a -> a -> (a,a,a)
xyz2rgb x y z = (f r, f g, f b)
  where r =  3.240479 * x - 1.537150 * y - 0.498535 * z
        g = -0.969256 * x + 1.875992 * y + 0.041556 * z
        b =  0.055648 * x - 0.204043 * y + 1.057311 * z
        f = f' . (/y'n)
        y'n = 100
        f' v | v <= 0.0031308 = 12.92 * v
             | otherwise      = 1.055 * (v**(1/2.4)) - 0.055

-- | Convert a CIE LUV colour to CIE XYZ.
-- Note that we fix the L* discontinuity at 8 instead of
-- CIE value. For a rationale, see
-- http://www.brucelindbloom.com/index.html?Eqn_Luv_to_XYZ.html
luv2xyz :: (Ord a, Fractional a) => a -> a -> a -> (a,a,a)
luv2xyz l u v = (x,y,z)
  where x = y * 9*u'/(4*v')
        y = y'n * if l <= 8 then l*(3/29)^3 else ((l+16)/116)^3
        --z = y*(12 - 3*u' - 20*v')/(4*v')
        z = -x/3 - 5*y + 3*y/v'
        u' = u/(13*l) + u'n
        v' = v/(13*l) + v'n
        y'n = 100
        u'n = 0.1978398
        v'n = 0.4683363

-- |Convert a CIE LChuv (cylindrical) colour to standard CIE LUV.
lch2luv :: Floating a => a -> a -> a -> (a,a,a)
lch2luv l c h = (l, c * cos (radians h), c * sin (radians h))

-- |Convert a CIE LCHuv (cylindrical) colour to sRGB.
lch2rgb :: (Ord a, Floating a) => a -> a -> a -> (a,a,a)
lch2rgb l c = uncurry3 xyz2rgb . uncurry3 luv2xyz . lch2luv l c

-- |Uncurry for triples.
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- |Convert an angle from degrees to radians.
radians :: Floating a => a -> a
radians = (*(pi/180))
