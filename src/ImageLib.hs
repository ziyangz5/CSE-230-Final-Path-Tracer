{-# LANGUAGE QuasiQuotes #-}
module ImageLib where
import Data.Array.Repa.Stencil
import Data.Array.Repa
import Data.Array.Repa.Stencil.Dim2
import qualified Data.Array.Repa.Repr.Unboxed as U

import Data.Array.Repa.IO.BMP (readImageFromBMP, writeImageToBMP)
import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Control.Monad ((>=>))


type RImage = Array U DIM2 Double

gaussainKernel :: Stencil DIM2 Double
gaussainKernel =
    [stencil2| 1   2   4   2   1 
               2   7   10  7   2 
               4   10  15  10  4 
               2   7   10  7   2 
               1   2   4   2   1  |]
normalize :: Double -> RImage -> IO RImage
normalize n = computeP . R.map (/ n)

gaussFilter :: RImage -> IO RImage
gaussFilter = computeP . mapStencil2 (BoundConst 255) gaussainKernel >=> normalize 119

toImageArray :: Array U DIM2 Word8 -> IO RImage
toImageArray = computeP . R.map (\x-> fromIntegral (fromIntegral x :: Int))

toWordImage :: RImage -> IO (Array U DIM2 Word8)
toWordImage = computeP . R.map (\x->fromIntegral (truncate x :: Int))


processImage :: String -> String -> IO ()
processImage infile outfile = do
        (r, g, b) <- fmap (either (error . show) U.unzip3) (readImageFromBMP infile)
        rimg <- mapM toImageArray [r,g,b]
        filtedImg <- mapM gaussFilter rimg
        [rr,gr,br] <- mapM toWordImage filtedImg
        writeImageToBMP outfile (U.zip3 rr gr br)
