module Shape where
import Distribution.SPDX (LicenseId(AFL_3_0))

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

rotate :: Shape -> Shape
rotate (Circle r) = Circle r
rotate (Rectangle w h) = Rectangle h w

scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)

