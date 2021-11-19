module Shader where
import RTPrimitive (Material(MkMaterial), Shape (Sphere, Triangle), Ray (Ray))
import Linear (V3 (V3), Metric (dot, norm), (^*), normalize, Additive ((^-^)), (^/))
import SceneCommand (getMaterial, Store)
import Debug.Trace
import BVH (BVHTree, getBVHClosetHit)
import Data.Maybe (isNothing, isJust)
import Utility ( epsilon )











---------------------------------------------------------------
-- Deprecated: Ray casting shader
-- Only use for testing
---------------------------------------------------------------


simpleRayCastShader :: Store -> BVHTree -> Shape -> V3 Float -> V3 Float -> Ray -> V3 Float
simpleRayCastShader scene bvh hshape hitPos normal (Ray org dir rtype) = if inshadow then ambient else (cDiffuse + cSpecular) ^/ r
    where
        MkMaterial ambient diffuse specular emission shininess = getShapeMaterial hshape
        plightLoc = V3 (-0.4) 0.4 1.0
        lightHit = plightLoc ^-^ hitPos
        r = norm lightHit
        lightDir = normalize lightHit
        cDiffuse = diffuse ^* max (normal `dot` lightDir)  0
        h = normalize(normalize (org - hitPos) + lightDir)
        cSpecular = specular ^* (max (h `dot` normal) 0 ** shininess)
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

-- >>> norm test
-- Variable not in scope: norm :: V3 Double -> t
-- 0.9999999704599998
