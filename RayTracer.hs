import Vector3D
import Data.Maybe (mapMaybe)
import GHC.Exts (sortWith)

data Shape = Sphere { radius::Float
                    , center::Vector3D
                    } deriving (Show, Eq)

type World = [Shape]

world :: World
world = [Sphere 200 (Vec 0 (-300) (-1200))]

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

intersect :: Shape -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
intersect s@(Sphere r c) eyePos rayDir =
  let eyeToC = eyePos - c in
  do
    n <- minroot (rayDir `dot` rayDir)
                 (2 * (eyeToC `dot` rayDir))
                 ((eyeToC `dot` eyeToC) - r^2)
    return $ (eyePos + (n `mult` rayDir), s)

firstHit :: World -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
firstHit w eyePos rayDir = case hits of
                             [] -> Nothing
                             (f:_) -> Just f
    where hits = sortWith (\(h, _) -> mag $ h - eyePos) $
                 mapMaybe (\s -> intersect s eyePos rayDir) w

normal :: Shape -> Vector3D -> Vector3D
normal (Sphere _ c) pt = signum $ c - pt

lambert :: Shape -> Vector3D -> Vector3D -> Float
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos
