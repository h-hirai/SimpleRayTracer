module Main where

import RayTracer
import World
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace world_c 0.5
