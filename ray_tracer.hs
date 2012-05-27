module Main where

import RayTracer
import WorldC
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace world (-96) 96 (-54) 54 0.5
