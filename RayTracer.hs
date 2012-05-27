module RayTracer where

import Data.Word (Word8)

import Vector3D
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Codec.BMP
import qualified Data.ByteString as B

data Color = Color { r::Word8 , g::Word8 , b::Word8} deriving (Show, Eq)

mulCol :: Float -> Color -> Color
mulCol a (Color r g b) = Color (mul a r) (mul a g) (mul a b)
    where mul f i = round $ f * fromIntegral i

packCol :: Color -> B.ByteString
packCol (Color r g b) = B.pack [r, g, b, 0]

white, red, green, blue :: Color
white = Color 255 255 255
red = Color 255 0 0
green = Color 0 255 0
blue = Color 0 0 255
purple = Color 255 0 255
cyan = Color 0 255 255

data Shape = Sphere { radius::Float
                    , center::Vector3D
                    , color::Color
                    } deriving (Show, Eq)

type World = [Shape]

minroot :: (Floating a, Ord a) => a -> a -> a -> Maybe a
minroot a b c
    | a == 0 =
        Just $ (-c) / b
    | disc >= 0 =
        Just $ min (((-b) + sqrt disc) / (2*a)) (((-b) - sqrt disc) / (2*a))
    | otherwise =
        Nothing
  where
    disc = (b^2) - (4*a*c)

intersect :: Shape -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
intersect s@(Sphere radius center _) cameraPos rayDir =
  let camToC = cameraPos - center in
  do
    n <- minroot (rayDir `dot` rayDir)
                 (2 * (camToC `dot` rayDir))
                 ((camToC `dot` camToC) - radius^2)
    return $ (cameraPos + (n `mult` rayDir), s)

firstHit :: World -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
firstHit w cameraPos rayDir =
    if null hits
    then Nothing
    else Just $ minimumBy (comparing $ \(h, _) -> mag $ h - cameraPos) hits
    where hits = mapMaybe (\s -> intersect s cameraPos rayDir) w

normal :: Shape -> Vector3D -> Vector3D
normal (Sphere _ c _) pt = signum $ c - pt

lambert :: Shape -> Vector3D -> Vector3D -> Float
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos

sendRay :: World -> Vector3D -> Vector3D -> Color
sendRay w cameraPos rayDir =
    case firstHit w cameraPos rayDir of
      Just (h, s) -> let l = lambert s h rayDir
                         c = color s in
                     l `mulCol` c
      Nothing -> Color 0 0 0

colorAt :: World -> Vector3D -> Float -> Float -> Color
colorAt w cameraPos x y = sendRay w cameraPos rayDir
    where rayDir = signum $ Vec x y 0 - cameraPos

trace :: World -> Vector3D -> Float -> Float -> Float -> Float -> Float -> BMP
trace w cameraPos startx endx starty endy step =
    let rows = [starty, starty + step .. endy]
        cols = [startx, startx + step .. endx]
        bs = B.concat [packCol $ colorAt w cameraPos col row |
                       row <- rows, col <- cols]
        height = length rows
        width = length cols in
    packRGBA32ToBMP width height bs
