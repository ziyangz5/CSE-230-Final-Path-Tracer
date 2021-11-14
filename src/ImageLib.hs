module ImageLib where

import Codec.Picture 
import Codec.Picture.Types (createMutableImage, freezeImage)
import Control.Monad
    
writeImage :: IO ()
writeImage = do
    img <- createMutableImage 120 120 (PixelRGB8 255 255 255)
    let coord = [0..99]
    forM_ coord $ \s -> do
        writePixel img s s (PixelRGB8 255 0 255) 
    img2save <- freezeImage img
    writePng "./test.png" img2save
