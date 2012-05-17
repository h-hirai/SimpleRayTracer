{-# LANGUAGE TemplateHaskell #-}

module RayTracerTH where

import Language.Haskell.TH

import RayTracer
import Vector3D

import Codec.BMP

runTrace :: World -> Vector3D ->
            Float -> Float -> Float -> Float -> Float ->
            ExpQ
runTrace w eyePos startx endx starty endy step = do
  runIO $ writeBMP "result.bmp" $ trace w eyePos startx endx starty endy step
  tupE []
