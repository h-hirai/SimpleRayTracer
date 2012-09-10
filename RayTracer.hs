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

type Screen = (Float, Float, Float, Float)
data World = World { shapes::[Shape]
                   , cameraPos::Vector3D
                   , raySrcPos::Vector3D
                   , screen::Screen
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
    disc = (b^2) - (4*a*c)

intersect :: Shape -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
intersect s@(Sphere radius center _) startPos dir =
  let camToC = startPos - center in
  do
    n <- minroot (dir `dot` dir)
                 (2 * (camToC `dot` dir))
                 ((camToC `dot` camToC) - radius^2)
    return $ (startPos + (n `mult` dir), s)


allHits :: [Shape] -> Vector3D -> Vector3D -> [(Vector3D, Shape)]
allHits shapes startPos dir = mapMaybe (\s -> intersect s startPos dir) shapes

firstHit :: [Shape] -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
firstHit shapes startPos dir =
    let hits = allHits shapes startPos dir in
    if null hits
    then Nothing
    else Just $ minimumBy (comparing $ \(h, _) -> mag $ h - startPos) hits

normal :: Shape -> Vector3D -> Vector3D
normal (Sphere _ c _) pt = signum $ c - pt

lambert :: Shape -> Vector3D -> Vector3D -> Float
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos

sendRay :: World -> Vector3D -> Color
sendRay w@(World shapes cameraPos raySrcPos _) cameraDir =
    maybe (Color 0 0 0) id $ do
      (hitPos, s) <- firstHit shapes cameraPos cameraDir
      let rayDir = signum $ hitPos - raySrcPos
      (hitPos', s') <- firstHit shapes raySrcPos rayDir
      if closeEnough hitPos hitPos' && s == s'
      then
          let l = lambert s hitPos rayDir
              c = color s in
          Just $ l `mulCol` c
      else
          Nothing
    where closeEnough v1 v2 = 1 > mag (v1 - v2)

colorAt :: World -> Float -> Float -> Color
colorAt w x y = sendRay w (signum $ Vec x y 0 - cameraPos w)

trace :: World -> Float -> BMP
trace w resolution =
    let (startx, endx, starty, endy) = screen w
        rows = [starty, starty + resolution .. endy]
        cols = [startx, startx + resolution .. endx]
        bs = B.concat [packCol $ colorAt w col row | row <- rows, col <- cols]
        height = length rows
        width = length cols in
    packRGBA32ToBMP width height bs
