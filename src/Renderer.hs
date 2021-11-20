module Renderer where
import SceneCommand (Store, getImgSize, getCamera, getShapeList, getMaterial, getDepth)
import Linear
import RTPrimitive (Camera(Camera), Ray (Ray), Shape, hasIntersection)
import Data.Maybe (isNothing)
import Data.Tuple.Select
import Utility ((@==@), getRand, getRandPair, capV3)
import Data.List ( foldl1',foldl', elemIndex )
import Shader
import Data.Word ( Word64)
import Random.MWC.Primitive
import BVH (BVHTree, getBVHClosetHit)
import qualified Debug.Trace
import Random.MWC.Pure (RangeRandom(range_random))
import Data.Bits
import System.Random.Mersenne.Pure64 (pureMT, PureMT)

rayCast:: Store -> BVHTree -> Int -> Int -> Int ->  V3 Float
rayCast scene bvh x y sindex = capV3 $ sampling scene bvh i j x y bseed
    where
        i = (fromIntegral x:: Float) + 0.5
        j = (fromIntegral y:: Float) + 0.5
        bseed =  fromIntegral (sindex*42 * (x*10+5) * (y*10+1)) `shiftL` 40 


sampling:: Store -> BVHTree -> Float -> Float-> Int -> Int -> Word64 -> V3 Float
sampling scene bvh i j x y bseed = singleRayShoot scene bvh (i + rnd1) (j + rnd2) rseed
                                where
                                    (rnd1,rnd2,rseed) = getRandPair $ pureMT bseed


singleRayShoot :: Store -> BVHTree  -> Float -> Float -> PureMT -> V3 Float
singleRayShoot scene bvh i j = rayTrace (Ray eye dir 0) bvh scene (getDepth scene)
    where
        (iwidth, iheight) = getImgSize scene
        (fwidht, fheight) = (fromIntegral iwidth:: Float,fromIntegral iheight:: Float)
        Camera eye center up w u v fovx fovy = getCamera scene
        alpha = tan(fovx / 2) * ((i - fwidht / 2.0) / (fwidht / 2.0))
        beta = tan(fovy / 2) * ((j - fheight / 2.0) / (fheight / 2.0))
        dir = (alpha *^ u) ^+^ (beta *^ v) ^-^ w




--getClosetCollision $ getCollisions ray (getShapeList scene)
        --hitResult = getClosetCollision $ getCollisions ray (getShapeList scene)

getClosetCollision :: [(Shape,V3 Float,V3 Float,Float)] -> Maybe (Shape,V3 Float,V3 Float,Float)
getClosetCollision [] = Nothing
getClosetCollision cs = Just (shape,hitPos,normal,t)
                        where
                            ts = map sel4 cs
                            minindex = elemIndex (foldl1' min ts) ts
                            (shape,hitPos,normal,t) = case minindex of {Nothing -> error "see renderer.hs";Just index -> cs!!index}

getCollisions:: Ray -> [Shape] -> [(Shape,V3 Float,V3 Float,Float)]
getCollisions ray [] = []

getCollisions ray (s:ss) =
    case result of
        Nothing -> getCollisions ray ss
        Just (hitPos,normal,t) -> (s, hitPos,normal,t) : getCollisions ray ss
    where
        result = hasIntersection ray s


test :: Maybe Int
test = elemIndex (foldl1' min [9.76567,1.123,2.777,3.456,sin 1.777]) [9.76567,1.123,2.777,3.456,sin 1.777]

-- >>> test
-- Just 4
