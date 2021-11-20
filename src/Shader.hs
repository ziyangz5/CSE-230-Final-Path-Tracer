module Shader where
import RTPrimitive (Material(MkMaterial), Shape (Sphere, Triangle), Ray (Ray))
import Linear (V3 (V3), Metric (dot, norm), (^*), normalize, Additive ((^-^), (^+^)), (^/), (*^), cross)
import SceneCommand (getMaterial, Store)
import Debug.Trace
import BVH (BVHTree, getBVHClosetHit)
import Data.Maybe (isNothing, isJust)
import Utility ( epsilon, getRandPair )
import System.Random.Mersenne.Pure64 (PureMT)

rayTrace:: Ray-> BVHTree -> Store -> Int -> PureMT -> V3 Float
rayTrace ray bvh scene depth rseed =
    if depth <= 0 then V3 0 0 0
    else
        case hitResult of
            Nothing ->  V3 0.8 0.9 0.89
            Just (shape,hitPos,normal,_) -> pathTracer scene bvh shape hitPos normal ray depth rseed
    where
        hitResult = getBVHClosetHit ray bvh 


pathTracer :: Store -> BVHTree -> Shape -> V3 Float -> V3 Float -> Ray -> Int -> PureMT -> V3 Float
pathTracer scene bvh hshape hitPos normal (Ray org dir rtype) depth rseed = 
    throughput * rayTrace (Ray (hitPos + omega_i ^* epsilon) omega_i 0) bvh scene (depth - 1) nrseed
    where
        MkMaterial ambient diffuse specular emission shininess = getShapeMaterial hshape
        r = normalize(dir - (2 * dot dir normal *^ normal))
        (omega_i,nrseed) = getHemiSample normal rseed
        throughput = 2 * pi *getPhongBRDF diffuse specular shininess omega_i r ^* max(dot normal omega_i) 0


getHemiSample :: V3 Float -> PureMT -> (V3 Float,PureMT)
getHemiSample normal rseed = (normalize omega_i, nrseed)
    where
        omega_i = (sx *^ u) ^+^ (sy *^ v) ^+^ (sz *^ w)
        (sx,sy,sz) = (cos phi * sin theta,sin phi * sin theta, cos theta)
        theta = acos rnd1
        phi = 2 * pi * rnd2
        w = normal
        a' = V3 0 1 0
        a = if norm (a' - w) < epsilon || norm (a' + w) < epsilon then V3 0 0 1 else a'
        (rnd1,rnd2,nrseed) = getRandPair rseed
        u = normalize (cross a w)
        v = cross w u



getPhongBRDF :: V3 Float -> V3 Float -> Float -> V3 Float -> V3 Float -> V3 Float
getPhongBRDF kd ks s omega_i r = if norm ks < epsilon
                                    then kd ^/ pi
                                    else kd ^/ pi + ks ^* ((s+2)/(2*pi)) ^* (max (dot r omega_i) 0 ** s)




---------------------------------------------------------------
-- Deprecated: Ray casting shader
-- Only use for testing
---------------------------------------------------------------


simpleRayCastShader :: Store -> BVHTree -> Shape -> V3 Float -> V3 Float -> Ray -> Int ->PureMT -> V3 Float
simpleRayCastShader scene bvh hshape hitPos normal (Ray org dir rtype) _ _ = if inshadow then ambient else color ^/ att
    where
        MkMaterial ambient diffuse specular emission shininess = getShapeMaterial hshape
        plightLoc = V3 1 1 3
        lightHit = plightLoc ^-^ hitPos
        att = norm lightHit
        r = normalize(dir - (2 * dot dir normal *^ normal))
        color = 2 * pi *getPhongBRDF diffuse specular shininess lightDir r ^* max(dot normal lightDir) 0
        lightDir = normalize lightHit

        inshadow = isInShadow (Ray (hitPos + normal ^* epsilon) lightDir 1) hitPos plightLoc bvh

getShapeMaterial:: Shape -> Material
getShapeMaterial shape = case shape of
                            {
                                Sphere _ _ mat _ _ _ -> mat;
                                Triangle _ _ _ mat _ _ _ -> mat;
                            }


isInShadow :: Ray -> V3 Float -> V3 Float -> BVHTree -> Bool
isInShadow ray@(Ray loc dir _) hitPos lightPos bvh = isJust hitResult && (
                                                    case hitResult of
                                                        Just (shape,sHitPos,normal,t) -> norm (hitPos ^-^ lightPos) > norm (hitPos ^-^ sHitPos)
                                                        _ -> False)
                        where
                            hitResult = getBVHClosetHit ray bvh

test :: V3 Double
test = V3 (-0.22269252) 0.1588609 0.9618582

-- >>> (V3 1 1 1) - 1
-- V3 0 0 0
--


-- >>> norm test
-- Variable not in scope: norm :: V3 Double -> t
-- 0.9999999704599998
