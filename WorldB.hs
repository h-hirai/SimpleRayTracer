module WorldB (world, cameraPos) where

import RayTracer
import Vector3D

ring :: World
ring = [Sphere 25 (Vec (x a) (y a) (z a)) (c a) | a <- [0..n-1]]
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

axis :: World
axis = [ Sphere 80 (Vec 0 0 (-700)) white
       , Sphere 10 (Vec (-100) 150 (-700)) purple
       , Sphere 10 (Vec 100 (-150) (-700)) cyan
       , Sphere 40 (Vec (-60) 90 (-700)) (0.8 `mulCol` purple)
       , Sphere 40 (Vec 60 (-90) (-700)) (0.8 `mulCol` cyan)
       , Sphere 900 (Vec (-520) 1040 (-700)) red
       , Sphere 900 (Vec 520 (-1040) (-700)) blue
       ]

world = ring ++ axis

cameraPos :: Vector3D
cameraPos = (Vec 0 0 200)
