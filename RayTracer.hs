import Vector3D

data SphereShape = Sphere { radius::Float
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

sphereIntersect :: SphereShape -> Vector3D -> Vector3D -> Maybe Vector3D
sphereIntersect sphere eyePos rayDir =
  let c = center sphere
      eyeToC = eyePos - c in
  do
    n <- minroot (rayDir `dot` rayDir)
                 (2 * (eyeToC `dot` rayDir))
                 ((eyeToC `dot` eyeToC) - (radius sphere)^2)
    return $ eyePos + (n `mult` rayDir)
