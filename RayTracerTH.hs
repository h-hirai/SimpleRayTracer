{-# LANGUAGE TemplateHaskell #-}

module RayTracerTH where

import Language.Haskell.TH
import Codec.BMP
import RayTracer
import WorldB

do
  runIO $ writeBMP "result.bmp" $
        trace world eyePos (-96) 96 (-54) 54 1.0
  return []
