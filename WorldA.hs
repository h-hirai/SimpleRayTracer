module WorldA (world, cameraPos) where

import RayTracer
import Vector3D

world :: World
world = [Sphere 200 (Vec 0 300 (-1200)) red
        ,Sphere 200 (Vec 80 150 (-1200)) green
        ,Sphere 200 (Vec (-70) 100 (-1200)) blue
        ] ++ [
         Sphere 40 (Vec (-200*x) (-300) (-400*z)) ((1/z) `mulCol` white)
         | x <- [-2..2], z <- [2..7]
        ]

cameraPos :: Vector3D
cameraPos = (Vec 0 0 200)
