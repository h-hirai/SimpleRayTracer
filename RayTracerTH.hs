{-# LANGUAGE TemplateHaskell #-}

module RayTracerTH where

import Language.Haskell.TH
import Codec.BMP
import RayTracer
import World

do
  runIO $ writeBMP "result.bmp" $ trace world_b 1.0
  return []
