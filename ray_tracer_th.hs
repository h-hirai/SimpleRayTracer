{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import RayTracerTH
import RayTracer (world_b, world_c, eyePos)
import Codec.BMP
import Data.ByteString (pack)

main :: IO ()
main =
  let startx = -96
      endx = 96
      starty = -54
      endy = 54
      step = 2.0
      width = 1 + floor ((endx - startx) / step)
      height = 1 + floor ((endy - starty) / step) in
  writeBMP "result.bmp" $
  packRGBA32ToBMP width height $
  pack $ $(trace (world_b ++ world_c) eyePos (-96) 96 (-54) 54 2.0)
