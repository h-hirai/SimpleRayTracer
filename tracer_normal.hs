import RayTracer
import Codec.BMP

main :: IO ()
main = writeBMP "result.bmp" $
       trace (world_b ++ world_c) eyePos (-96) 95 (-54) 53 0.2
