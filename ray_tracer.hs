module Main where

import RayTracer
import World
import Codec.BMP
import System.Environment (getArgs)

main :: IO ()
main = do
  [w, res, outf] <- getArgs
  writeBMP outf $ trace (case w of "world_a" -> world_a
                                   "world_b" -> world_b
                                   "world_c" -> world_c) (read res)
