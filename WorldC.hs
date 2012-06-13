module WorldC (world, cameraPos) where

import RayTracer
import Vector3D

ring :: [Shape]
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

axis :: [Shape]
axis = [ Sphere 150 (Vec 0 0 (-700)) white
       , Sphere 800 (Vec 400 0 (-3000)) red
       , Sphere 600 (Vec (-400) 0 (-3000)) blue
       ]

screenSize = (-96, 96, -54, 54)

world = World (ring ++ axis) (Vec 0 0 200) (Vec (-50) (-50) 150) screenSize
