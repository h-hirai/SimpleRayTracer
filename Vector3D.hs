module Vector3D where

data Num a => Vector3D a = Vec a a a deriving (Show, Eq)

mag :: Floating a => Vector3D a -> a
mag (Vec x y z) = sqrt (x^2 + y^2 + z^2)

scalarMult :: Floating a => a -> Vector3D a -> Vector3D a
scalarMult k (Vec x y z) = Vec (k*x) (k*y) (k*z)

dotProduct :: Floating a => Vector3D a -> Vector3D a -> a
dotProduct (Vec a1 a2 a3) (Vec b1 b2 b3) = a1*b1 + a2*b2 + a3*b3

instance Floating a => Num (Vector3D a) where
    (Vec a1 a2 a3) + (Vec b1 b2 b3) = Vec (a1 + b1) (a2 + b2) (a3 + b3)
    (Vec a1 a2 a3) - (Vec b1 b2 b3) = Vec (a1 - b1) (a2 - b2) (a3 - b3)
    (Vec a1 a2 a3) * (Vec b1 b2 b3) =
        Vec (a2*b3 - a3*b2) (a3*b1 - b1*b3) (a1*b2 - a2*b1)
    negate (Vec a1 a2 a3) = Vec (negate a1) (negate a2) (negate a3)
    abs (Vec a1 a2 a3) = Vec (abs a1) (abs a2) (abs a3)
    signum v@(Vec x y z) = let d = mag v in Vec (x / d) (y / d) (z / d)
    fromInteger n = Vec (fromInteger n) 0 0
