module Vector3D where

data Vector3D = Vec { x::Float , y::Float , z::Float } deriving (Show, Eq)

mag :: Vector3D -> Float
mag (Vec x y z) = sqrt (x^2 + y^2 + z^2)

mult :: Float -> Vector3D -> Vector3D
mult k (Vec x y z) = Vec (k*x) (k*y) (k*z)

dot :: Vector3D -> Vector3D -> Float
dot (Vec a1 a2 a3) (Vec b1 b2 b3) = a1*b1 + a2*b2 + a3*b3

instance Num Vector3D where
    (Vec a1 a2 a3) + (Vec b1 b2 b3) = Vec (a1 + b1) (a2 + b2) (a3 + b3)
    (Vec a1 a2 a3) - (Vec b1 b2 b3) = Vec (a1 - b1) (a2 - b2) (a3 - b3)
    (Vec a1 a2 a3) * (Vec b1 b2 b3) =
        Vec (a2*b3 - a3*b2) (a3*b1 - b1*b3) (a1*b2 - a2*b1)
    negate (Vec a1 a2 a3) = Vec (-a1) (-a2) (-a3)
    abs (Vec a1 a2 a3) = undefined
    signum v@(Vec x y z) = let d = mag v in Vec (x / d) (y / d) (z / d)
    fromInteger n = undefined
