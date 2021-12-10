{-# LANGUAGE TypeOperators #-}
module Main where

import SceneParser ( runFile )
import ImageLib
import Data.Array.Repa as R hiding ((++))
import Codec.Picture.Repa (Img(Img))
import Codec.Picture.Extra (flipVertically)
import Data.Word (Word8, Word64)
import Codec.Picture
import Linear (V3(V3))
import SceneCommand
import Renderer ( rayCast )

import Data.List (unfoldr)
import BVH (buildBVH, BVHTree)
import Control.Monad.State (evalState)
import Control.Monad (when)
import ImageLib (processImage)
import System.Environment (getArgs)

type RGB8 = (Pixel8, Pixel8, Pixel8)

type FRGB8 = (Float, Float, Float)


main :: IO ()
main = do
          args <- getArgs
          if length args < 2 then
            putStrLn "Incorrect number of arguments.(<path> <sampleNumber>)"
          else
            do
              let (path,strssn) = (head args,last args)
              putStrLn "Initializing scene.."
              (scene,flag) <- runFile path;
              if flag then
                do
                  let bvh = evalState (buildBVH (getShapeList scene)) 0
                  putStrLn "Initialized."
                  putStrLn "Begin ray-tracing.."
                  let sampleNum = read strssn
                  let snf = fromIntegral sampleNum
                  fimg <-  rayTraceSampling sampleNum scene bvh
                  img <- computeP $ R.map (\rgb -> colorMapping rgb snf) fimg
                  putStrLn "Saving.."
                  (saveBmpImage ("./" ++ getPath scene ++ ".bmp") . ImageRGB8. flipVertically . toImage) img
                  putStrLn "RT Done"
                  processImage ("./" ++ getPath scene ++ ".bmp") ("./" ++ getPath scene ++ "_pp.bmp")
                  putStrLn "Post-processing Done"
              else
                  do
                    putStrLn "Scene file has syntax error. Please check."

colorMapping :: (Float,Float,Float)->Float -> RGB8
colorMapping (r,g,b) snf = (fromIntegral $ min 255 (round $ ((r/snf)**(1/2.2))*255),
                            fromIntegral $ min 255 (round $ ((g/snf)**(1/2.2))*255),
                            fromIntegral $ min 255 (round $ ((b/snf)**(1/2.2))*255))

rayTraceSampling :: Int -> Store -> BVHTree -> IO(Array U DIM2 FRGB8)
rayTraceSampling scount scene bvh =
  do
    when ((scount) `mod` 5 == 0) $ putStrLn ("Remaining Samples: " ++ show scount)
    img1 <- R.computeUnboxedP $ generateImgRepa scene bvh scount
    if (scount-1) > 0
      then
        do
          img2 <- rayTraceSampling (scount-1) scene bvh
          computeP $ R.zipWith (\(r1,g1,b1) (r2,g2,b2)-> (r1+r2,g1+g2,b1+b2)) img1 img2
      else
         return img1


toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b


generateImgRepa :: Store -> BVHTree -> Int  -> Array D DIM2 FRGB8
generateImgRepa scene bvh scount = R.fromFunction (Z :. w :. h) (originalFnc' scene bvh scount)
                        where
                          (w,h) = getImgSize scene

originalFnc' :: Store ->BVHTree-> Int -> (Z :. Int :. Int) -> FRGB8
originalFnc' scene bvh scount (Z :. x :. y) = (r,g,b)
                                        where
                                                V3 r g b = rayCast scene bvh x y scount

