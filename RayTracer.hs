module Main where

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

data Shape = Sphere { radius::Float
                    , center::Vector3D
                    , color::Color
                    } deriving (Show, Eq)

type World = [Shape]

world :: World
world = [Sphere 200 (Vec 0 300 (-1200)) red
        ,Sphere 200 (Vec 80 150 (-1200)) green
        ,Sphere 200 (Vec (-70) 100 (-1200)) blue
        ] ++ [
         Sphere 40 (Vec (-200*x) (-300) (-400*z)) ((1/z) `mulCol` white)
         | x <- [-2..2], z <- [2..7]
        ]

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
  packRGBA32ToBMP width height $
  B.concat [packCol $ colorAt w eyePos col row |
          row <- [starty, starty + step .. endy],
          col <- [startx, startx + step .. endx]]
  where
    width = 1 + floor ((endx - startx) / step)
    height = 1 + floor ((endy - starty) / step)

main :: IO ()
main = writeBMP "result.bmp" $ trace world (Vec 0 0 200) (-96) 95 (-54) 53 0.2
