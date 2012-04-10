module Main where

import Data.Word (Word8)

import Vector3D
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Codec.BMP
import Data.ByteString (pack)

data Shape = Sphere { radius::Float
                    , center::Vector3D
                    } deriving (Show, Eq)

type World = [Shape]

world :: World
world = [Sphere 200 (Vec 0 300 (-1200))
        ,Sphere 200 (Vec 80 150 (-1200))
        ,Sphere 200 (Vec (-70) 100 (-1200))
        ] ++ [
         Sphere 40 (Vec (-200*x) (-300) (-400*z)) | x <- [-2..2], z <- [2..7]
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
intersect s@(Sphere r c) eyePos rayDir =
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
normal (Sphere _ c) pt = signum $ c - pt

lambert :: Shape -> Vector3D -> Vector3D -> Float
lambert s hitPos rayDir = max 0 $ rayDir `dot` normal s hitPos

sendRay :: World -> Vector3D -> Vector3D -> Float
sendRay w eyePos rayDir = case firstHit w eyePos rayDir of
                            Just (h, s) -> lambert s h rayDir
                            Nothing -> 0

colorAt :: World -> Vector3D -> Float -> Float -> Word8
colorAt w eyePos x y = round $ sendRay w eyePos rayDir * 255
    where rayDir = signum $ Vec x y 0 - eyePos

trace :: World -> Vector3D -> Float -> Float -> Float -> Float -> Float -> BMP
trace w eyePos startx endx starty endy step =
  packRGBA32ToBMP width height $
  pack $
  concatMap gray [colorAt w eyePos col row |
                  row <- [starty, starty + step .. endy],
                  col <- [startx, startx + step .. endx]]
  where
    width = 1 + floor ((endx - startx) / step)
    height = 1 + floor ((endy - starty) / step)
    gray y = [y,y,y,0]

main :: IO ()
main = writeBMP "result.bmp" $ trace world (Vec 0 0 200) (-96) 95 (-54) 53 0.2
