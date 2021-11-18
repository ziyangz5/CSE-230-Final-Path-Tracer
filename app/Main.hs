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

type RGB8 = (Pixel8, Pixel8, Pixel8)




-- main :: IO ()
-- main = do
--           putStrLn "Initializing scene.."
--           scene <- runFile "Scene/scene1.test";
--           putStrLn "Initialized."
--           putStrLn "Begin ray-tracing.."
--           img <- R.computeUnboxedP (generateImgRepa scene) 
--           putStrLn "Saving.."
--           (savePngImage "./test.png" . ImageRGB8. flipVertically . toImage) img
--           putStrLn "Done"

main :: IO ()
main = do
        scene <- runFile "Scene/scene1.test";
        let (iw,ih) = getImgSize scene
        let x = fromListUnboxed (Z :. (iw::Int) :. (ih::Int)) (replicate (iw *ih) (0,0,0)) :: Array U DIM2 RGB8
        res <- computeP $ R.traverse x id (beginRayTrace scene) :: IO (Array U DIM2 RGB8)
        (savePngImage "./test.png" . ImageRGB8. flipVertically . toImage) res
        putStrLn "Done"

toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b


generateImgRepa :: Store -> Array D DIM2 RGB8
generateImgRepa scene = R.fromFunction (Z :. w :. h) (originalFnc' scene)
                        where
                          (w,h) = getImgSize scene

originalFnc' :: Store -> (Z :. Int :. Int) -> RGB8
originalFnc' scene (Z :. x :. y) = (fromIntegral (round $ r * 255),fromIntegral (round $ g * 255),fromIntegral (round $ b * 255))
                                        where
                                                V3 r g b = rayCast scene x y

beginRayTrace :: Store -> (DIM2 -> RGB8) -> DIM2-> (Pixel8, Pixel8, Pixel8)
beginRayTrace scene _ (Z :. i :. j) = (fromIntegral (round $ r * 255),fromIntegral (round $ g * 255),fromIntegral (round $ b * 255))
                                        where
                                                V3 r g b = rayCast scene i j

-- >>>tesdt
-- Data.Array.Repa.Eval.Fill.fromList: provide array shape does not match list length
