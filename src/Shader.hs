module Shader where
import RTPrimitive (Material(MkMaterial), Shape (Sphere, Triangle), Ray (Ray))
import Linear (V3 (V3), Metric (dot, norm), (^*), normalize, Additive ((^-^), (^+^)), (^/), (*^), cross)
import SceneCommand (getMaterial, Store)
import Debug.Trace
import BVH (BVHTree, getBVHClosetHit)
import Data.Maybe (isNothing, isJust)
import Utility ( epsilon, getRandPair, getRand, average3, sepsilon, (@==@) )
import System.Random.Mersenne.Pure64 (PureMT)

rayTrace:: Ray-> BVHTree -> Store -> Int -> PureMT -> V3 Float
rayTrace ray bvh scene depth rseed =
    if depth <= 0 then V3 0 0 0
    else
        case hitResult of
            Nothing ->  V3 0.8 0.9 0.93
            Just (shape,hitPos,normal,_) -> pathTracer scene bvh shape hitPos normal ray depth rseed
    where
        hitResult = getBVHClosetHit ray bvh


pathTracer :: Store -> BVHTree -> Shape -> V3 Float -> V3 Float -> Ray -> Int -> PureMT -> V3 Float
pathTracer scene bvh hshape hitPos normal (Ray org dir rtype) depth rseed
  | norm emission > 0 = emission
  | rr /= rr|| (rg /= rg)||(rb /= rb) = V3 0 0 0
  | otherwise = result
  where
      result@(V3 rr rg rb)
        = if (dot omega_i normal < 0
                || dot omega_i (normalize (omega_i - dir)) < 0)
               || (pdf < sepsilon) then
              V3 0 0 0
          else
              (throughput
                 * rayTrace
                     (Ray (hitPos + omega_i ^* epsilon) omega_i 0) bvh scene (depth - 1)
                     nrseed)
                ^/ pdf
      MkMaterial ambient diffuse specular emission shininess
        = getShapeMaterial hshape
      r = normalize (dir - 2 * dot dir normal *^ normal)
      (omega_i, nrseed) = getGGXSample normal (- dir) shininess t rseed
      throughput
        = getGGXBRDF diffuse specular shininess normal omega_i (- dir) r
            ^* max (dot normal omega_i) 0
      t = max 0.25 (average3 specular)
            / (average3 specular + average3 diffuse)
      pdf = getGGXPdf normal omega_i (- dir) r shininess t

getGGXPdf :: V3 Float ->V3 Float -> V3 Float -> V3 Float ->Float-> Float -> Float
getGGXPdf normal omega_i omega_o r alpha t = d + (t * (_D * nhCosine))/ (4 * dot h omega_i)
    where
        h = normalize (omega_i ^+^ omega_o)
        d = (1-t) * max (dot normal omega_i) 0 /pi
        nhCosine = max (dot normal h)  0
        _D = getGGXMicroDistr h normal alpha



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

getCosineSample :: V3 Float -> PureMT -> (V3 Float,PureMT)
getCosineSample normal rseed = (normalize omega_i, nrseed)
    where
        omega_i = (sx *^ u) ^+^ (sy *^ v) ^+^ (sz *^ w)
        (sx,sy,sz) = (cos phi * sin theta,sin phi * sin theta, cos theta)
        theta = acos $ sqrt rnd1
        phi = 2 * pi * rnd2
        w = normal
        a' = V3 0 1 0
        a = if norm (a' - w) < epsilon || norm (a' + w) < epsilon then V3 0 0 1 else a'
        (rnd1,rnd2,nrseed) = getRandPair rseed
        u = normalize (cross a w)
        v = cross w u




getGGXSample :: V3 Float -> V3 Float -> Float-> Float -> PureMT  -> (V3 Float,PureMT)
getGGXSample normal omega_o alpha t rseed  = if xi_0 > t then getCosineSample normal rseed else (normalize omega_i, nrseed2)
    where
        omega_i = 2 * dot omega_o h *^ h - omega_o
        h = (hx *^ u) ^+^ (hy *^ v) ^+^ (hz *^ w)
        (hx,hy,hz) = (cos phi * sin theta,sin phi * sin theta, cos theta)
        theta = atan (alpha * sqrt xi_1/ sqrt (1-xi_1))
        phi = 2 * pi * rnd3
        w = normal
        a' = V3 0 1 0
        a = if norm (a' - w) < epsilon || norm (a' + w) < epsilon then V3 0 0 1 else a'
        (xi_0,xi_1,nrseed) = getRandPair rseed
        (rnd3,nrseed2) = getRand nrseed
        u = normalize (cross a w)
        v = cross w u



getPhongBRDF :: V3 Float -> V3 Float -> Float -> V3 Float -> V3 Float -> V3 Float
getPhongBRDF kd ks s omega_i r = if norm ks < epsilon
                                    then kd ^/ pi
                                    else kd ^/ pi + ks ^* ((s+2)/(2*pi)) ^* (max (dot r omega_i) 0 ** s)


getGGXShadowTerm :: V3 Float -> V3 Float -> Float -> Float
getGGXShadowTerm omega normal alpha = if vncosine <= 0 then 0 else 2 / (1 + sqrt (1 + alpha * alpha * (tan theta **2)))
    where
        theta = acos (min vncosine 1)
        vncosine = dot omega normal

getGGXMicroDistr :: V3 Float -> V3 Float ->Float -> Float
getGGXMicroDistr h normal alpha = alpha2 / denominator
    where
        theta = acos $ min 1 (dot h normal)
        alpha2 = alpha * alpha
        denominator = pi * (cos theta **4) * ((alpha2 + tan theta **2)**2)

getGGXBRDF :: V3 Float -> V3 Float -> Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
getGGXBRDF kd ks alpha normal omega_i omega_o r = if icosine <= 0 || ocosine <= 0 then V3 0 0 0 else  (kd ^/ pi) ^+^ ggxTerm
        where
            icosine = dot omega_i normal
            ocosine = dot omega_o normal
            cosineTerm = 4 * icosine * ocosine
            h = normalize (omega_i + omega_o)
            _F = ks + (1 - ks) ^* ((1 - max (dot (normalize omega_i)  h) 0 ) ** 5)
            _G = getGGXShadowTerm omega_i normal alpha * getGGXShadowTerm omega_o normal alpha
            _D = getGGXMicroDistr h normal alpha
            ggxTerm = (_F^*_G ^*_D) ^/ cosineTerm





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
        r = normalize(dir - 2 * dot dir normal *^ normal)
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

-- >>> 1 - (V3 2 1 1) 
-- V3 (-1) 0 0
--


-- >>> (-2.7) ** 4
-- 53.144100000000016
--
