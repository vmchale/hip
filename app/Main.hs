module Main where

import Graphics.Image as I



main :: IO ()
main = do
  let a = setComp Seq $
            makeImage (255 :. 255) (\(_ :. i) -> PixelY (fromIntegral i)) :: Image Y Word64
  displayImage (downsampleRows (upsampleRows a))
  
