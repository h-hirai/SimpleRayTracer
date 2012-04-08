import Vector3D

data Shape = Sphere { radius::Float
                    , center::Vector3D
                    }

minroot :: (Floating a, Ord a) => a -> a -> a -> Maybe a
minroot a b c
    | a == 0 =
        Just $ (-c) / b
    | disc >= 0 =
        Just $ min (((-b) + sqrt disc) / (2*a)) (((-b) - sqrt disc) / (2*a))
    | otherwise =
        Nothing
  where
    disc = (b^2) - (a*c)

intersect :: Shape -> Vector3D -> Vector3D -> Maybe Vector3D
intersect (Sphere r c) eyePos rayDir =
  let eyeToC = eyePos - c in
  do
    n <- minroot (rayDir `dot` rayDir)
                 (2 * (eyeToC `dot` rayDir))
                 ((eyeToC `dot` eyeToC) - r^2)
    return $ eyePos + (n `mult` rayDir)
