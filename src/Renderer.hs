module Renderer where
import SceneCommand (Store, getImgSize, getCamera, getShapeList, getMaterial)
import Linear
import RTPrimitive (Camera(Camera), Ray (Ray), Shape, hasIntersection)
import Data.Maybe (isNothing)
import Data.Tuple.Select
import Utility ((@==@), getRand)
import Data.List ( foldl1',foldl', elemIndex )
import Shader (simpleTestingShader)
import Data.Word ( Word32)
import Random.MWC.Primitive (Seed, seed)

rayCast:: Store -> Int -> Int ->  V3 Float
rayCast scene x y = sampling scene i j (seed bseed) sampleNum ^/ sampleNum
    where
        i = (fromIntegral x:: Float) + 0.5
        j = (fromIntegral y:: Float) + 0.5
        sampleNum = 150.0
        bseed = [fromIntegral x,fromIntegral y]

-- rayShoot:: Store -> Int -> Int ->  V3 Float
-- rayShoot scene x y = singleRayShoot scene i j 
--     where
--         i = (fromIntegral x:: Float) + 0.5
--         j = (fromIntegral y:: Float) + 0.5
--         sampleNum = 25
--         bseed = [fromIntegral x,fromIntegral y]

sampling:: Store -> Float -> Float -> Seed -> Float ->V3 Float
sampling scene i j seed count = if count > 0
                                    then singleRayShoot scene (i + rnd1/2) (j + rnd2/2) + sampling scene i j nseed2 (count-1)
                                    else 0
                                where
                                    (rnd1,nseed) = getRand seed
                                    (rnd2,nseed2) = getRand nseed


singleRayShoot :: Store -> Float -> Float ->  V3 Float
singleRayShoot scene i j = rayTrace (Ray eye dir 0) scene
    where
        (iwidth, iheight) = getImgSize scene
        (fwidht, fheight) = (fromIntegral iwidth:: Float,fromIntegral iheight:: Float)
        Camera eye center up w u v fovx fovy = getCamera scene
        alpha = tan(fovx / 2) * ((i - fwidht / 2.0) / (fwidht / 2.0))
        beta = tan(fovy / 2) * ((j - fheight / 2.0) / (fheight / 2.0))
        dir = (alpha *^ u) ^+^ (beta *^ v) ^-^ w




rayTrace:: Ray -> Store -> V3 Float
rayTrace ray scene =
    case hitResult of
        Nothing ->  V3 0 0 0
        Just (shape,hitPos,normal) -> simpleTestingShader scene shape hitPos normal ray

    where
        hitResult = getClosetCollision $ getCollisions ray (getShapeList scene)

getClosetCollision :: [(Shape,V3 Float,V3 Float,Float)] -> Maybe (Shape,V3 Float,V3 Float)
getClosetCollision [] = Nothing
getClosetCollision cs = Just (shape,hitPos,normal)
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
