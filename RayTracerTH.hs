{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module RayTracerTH where

import Language.Haskell.TH

import RayTracer hiding (trace, packCol)

import Data.Word (Word8)
import Vector3D

packCol :: Color -> [Word8]
packCol (Color r g b) = [r, g, b, 0]

trace :: World -> Vector3D ->
         Float -> Float -> Float -> Float -> Float ->
         ExpQ
trace w eyePos startx endx starty endy step = do
  let ws = concat [packCol $ colorAt w eyePos col row |
                   row <- [starty, starty + step .. endy],
                   col <- [startx, startx + step .. endx]]
  listE $ map (litE . integerL . fromIntegral) ws
