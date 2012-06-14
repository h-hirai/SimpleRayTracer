module RayTracer where

import Data.Word (Word8)

import Vector3D
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Codec.BMP
import qualified Data.ByteString as B

data Color = Color { r::Word8 , g::Word8 , b::Word8} deriving (Show, Eq)

mulCol :: (RealFrac a) => a -> Color -> Color
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

data Shape a = Sphere { radius::a
                      , center::Vector3D a
                      , color::Color
                      } deriving (Show, Eq)

type Screen a = (a, a, a, a)
data World a = World { shapes::[Shape a]
                     , cameraPos::Vector3D a
                     , raySrcPos::Vector3D a
                     , screen::Screen a
                     } deriving (Show, Eq)

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

intersect :: (Floating a, Ord a) =>
             Shape a -> Vector3D a -> Vector3D a -> Maybe (Vector3D a, Shape a)
intersect s@(Sphere radius center _) startPos dir =
  let camToC = startPos - center in
  do
    n <- minroot (dir `dot` dir)
                 (2 * (camToC `dot` dir))
                 ((camToC `dot` camToC) - radius^2)
    return $ (startPos + (n `mult` dir), s)


allHits :: (Floating a, Ord a) =>
           [Shape a] -> Vector3D a -> Vector3D a -> [(Vector3D a, Shape a)]
allHits shapes startPos dir = mapMaybe (\s -> intersect s startPos dir) shapes

firstHit :: (Floating a, Ord a) =>
            [Shape a] -> Vector3D a -> Vector3D a -> Maybe (Vector3D a, Shape a)
firstHit shapes startPos dir =
    let hits = allHits shapes startPos dir in
    if null hits
    then Nothing
    else Just $ minimumBy (comparing $ \(h, _) -> mag $ h - startPos) hits

normal :: (Floating a) =>
          Shape a -> Vector3D a -> Vector3D a
normal (Sphere _ c _) pt = signum $ c - pt

lambert :: (Floating a, Ord a) =>
           Shape a -> Vector3D a -> Vector3D a -> a
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos

sendRay :: (Floating a, Ord a, RealFrac a) =>
           World a -> Vector3D a -> Color
sendRay w@(World shapes cameraPos raySrcPos _) cameraDir =
    case firstHit shapes cameraPos cameraDir of
      Just (hitPos, s) ->
          let rayDir = signum $ hitPos - raySrcPos in
          case firstHit shapes raySrcPos rayDir of
            Just (hitPos', s')
              | closeEnough hitPos hitPos' && s == s' ->
                  let l = lambert s hitPos rayDir
                      c = color s in
                  l `mulCol` c
            _ -> Color 0 0 0
            where closeEnough v1 v2 = 1 > mag (v1 - v2)
      Nothing -> Color 0 0 0

colorAt :: (Floating a, Ord a, RealFrac a) =>
           World a -> a -> a -> Color
colorAt w x y = sendRay w (signum $ Vec x y 0 - cameraPos w)

trace :: (Floating a, RealFrac a, Enum a) =>
         World a -> a -> BMP
trace w resolution =
    let (startx, endx, starty, endy) = screen w
        rows = [starty, starty + resolution .. endy]
        cols = [startx, startx + resolution .. endx]
        bs = B.concat [packCol $ colorAt w col row | row <- rows, col <- cols]
        height = length rows
        width = length cols in
    packRGBA32ToBMP width height bs
