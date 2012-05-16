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

world_a :: World
world_a = [Sphere 200 (Vec 0 300 (-1200)) red
          ,Sphere 200 (Vec 80 150 (-1200)) green
          ,Sphere 200 (Vec (-70) 100 (-1200)) blue
          ] ++ [
           Sphere 40 (Vec (-200*x) (-300) (-400*z)) ((1/z) `mulCol` white)
           | x <- [-2..2], z <- [2..7]
          ]

world_b :: World
world_b = [Sphere 25 (Vec (x a) (y a) (z a)) (c a)
           | a <- [0..n-1]]
    where
      n = 20
      r = 300
      step a = fromIntegral a * 2 * pi / fromIntegral n
      x a = r * sin (step a)
      y a = x a / 2 + (z a + 700) / 5
      z a = r * cos (step a) - 700
      c a = let x = sin $ step a
                z = cos $ step a in
            if x > 0
            then Color (round $ 255 * x) (round $ 255 * abs z) 0
            else Color 0 (round $ 255 * abs z) (round $ 255 * (-x))

world_c :: World
world_c = [ Sphere 80 (Vec 0 0 (-700)) white
          , Sphere 10 (Vec (-100) 150 (-700)) purple
          , Sphere 10 (Vec 100 (-150) (-700)) cyan
          , Sphere 40 (Vec (-60) 90 (-700)) (0.8 `mulCol` purple)
          , Sphere 40 (Vec 60 (-90) (-700)) (0.8 `mulCol` cyan)
          , Sphere 900 (Vec (-520) 1040 (-700)) red
          , Sphere 900 (Vec 520 (-1040) (-700)) blue
          ]

eyePos :: Vector3D
eyePos = (Vec 0 0 200)

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
intersect s@(Sphere r c _) eyePos rayDir =
  let eyeToC = eyePos - c in
  do
    n <- minroot (rayDir `dot` rayDir)
                 (2 * (eyeToC `dot` rayDir))
                 ((eyeToC `dot` eyeToC) - r^2)
    return $ (eyePos + (n `mult` rayDir), s)

firstHit :: World -> Vector3D -> Vector3D -> Maybe (Vector3D, Shape)
firstHit w eyePos rayDir =
    if null hits
    then Nothing
    else Just $ minimumBy (comparing $ \(h, _) -> mag $ h - eyePos) hits
    where hits = mapMaybe (\s -> intersect s eyePos rayDir) w

normal :: Shape -> Vector3D -> Vector3D
normal (Sphere _ c _) pt = signum $ c - pt

lambert :: Shape -> Vector3D -> Vector3D -> Float
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos

sendRay :: World -> Vector3D -> Vector3D -> Color
sendRay w eyePos rayDir =
    case firstHit w eyePos rayDir of
      Just (h, s) -> let l = lambert s h rayDir
                         c = color s in
                     l `mulCol` c
      Nothing -> Color 0 0 0

colorAt :: World -> Vector3D -> Float -> Float -> Color
colorAt w eyePos x y = sendRay w eyePos rayDir
    where rayDir = signum $ Vec x y 0 - eyePos

trace :: World -> Vector3D -> Float -> Float -> Float -> Float -> Float -> BMP
trace w eyePos startx endx starty endy step =
    let rows = [starty, starty + step .. endy]
        cols = [startx, startx + step .. endx]
        bs = B.concat [packCol $ colorAt w eyePos col row |
                       row <- rows, col <- cols]
        height = length rows
        width = length cols in
    packRGBA32ToBMP width height bs
