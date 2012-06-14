module World (world_a, world_b, world_c) where

import RayTracer
import Vector3D

ss :: (Floating a, RealFrac a, Enum a) => [Shape a]
ss = [Sphere 200 (Vec 0 300 (-1200)) red
     ,Sphere 200 (Vec 80 150 (-1200)) green
     ,Sphere 200 (Vec (-70) 100 (-1200)) blue
     ] ++ [
      Sphere 40 (Vec (-200*x) (-300) (-400*z)) ((1/z) `mulCol` white)
      | x <- [-2..2], z <- [2..7]
     ]

ring :: (Floating a) => [Shape a]
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

axis :: (Floating a) => [Shape a]
axis = [ Sphere 80 (Vec 0 0 (-700)) white
       , Sphere 10 (Vec (-100) 150 (-700)) purple
       , Sphere 10 (Vec 100 (-150) (-700)) cyan
       , Sphere 40 (Vec (-60) 90 (-700)) (0.8 `mulCol` purple)
       , Sphere 40 (Vec 60 (-90) (-700)) (0.8 `mulCol` cyan)
       , Sphere 900 (Vec (-520) 1040 (-700)) red
       , Sphere 900 (Vec 520 (-1040) (-700)) blue
       ]

dual :: (Floating a) => [Shape a]
dual = [ Sphere 150 (Vec 0 0 (-700)) white
       , Sphere 800 (Vec 400 0 (-3000)) red
       , Sphere 600 (Vec (-400) 0 (-3000)) blue
       ]

screenSize = (-96, 96, -54, 54)
camPos =  (Vec 0 0 200)
raySrcPosA = (Vec 0 0 200)
raySrcPosB = (Vec (-50) (-50) 150)

world_a = World ss camPos raySrcPosA screenSize
world_b = World (ring ++ axis) camPos raySrcPosA screenSize
world_c = World (ring ++ dual) camPos raySrcPosB screenSize
