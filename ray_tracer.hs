module Main where

import RayTracer
import WorldB
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace world eyePos (-96) 96 (-54) 54 0.2
