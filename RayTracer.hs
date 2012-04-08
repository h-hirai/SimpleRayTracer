import Vector3D

distance ::  Floating a => Vector3D a -> Vector3D a -> a
distance a b = mag $ a - b

minroot :: (Floating a, Ord a) => a -> a -> a -> Maybe a
minroot a b c
    | a == 0 = Just $ (-c) / b
    | otherwise =
        let disc = (b^2) - (a*c) in
        if disc >= 0
        then Just $
             min (((-b) + sqrt disc) / (2*a)) (((-b) - sqrt disc) / (2*a))
        else Nothing
