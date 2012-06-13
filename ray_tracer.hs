module Main where

import RayTracer
import WorldC
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace world 0.5
