module Main where

import RayTracer
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace (world_b ++ world_c) eyePos (-96) 96 (-54) 54 2.0
