{-# LANGUAGE TemplateHaskell #-}

module RayTracerTH where

import Language.Haskell.TH
import Codec.BMP
import RayTracer

do
  runIO $ writeBMP "result.bmp" $
        trace (world_b ++ world_c) eyePos (-96) 96 (-54) 54 1.0
  return []
