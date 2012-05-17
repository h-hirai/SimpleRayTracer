{-# LANGUAGE TemplateHaskell #-}

module Main where

import RayTracerTH
import RayTracer (world_b, world_c, eyePos)

main :: IO ()
main = return $(runTrace (world_b ++ world_c) eyePos (-96) 96 (-54) 54 1.0)
