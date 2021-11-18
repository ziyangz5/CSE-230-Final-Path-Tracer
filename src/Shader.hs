module Shader where
import RTPrimitive (Material(MkMaterial), Shape (Sphere, Triangle), Ray (Ray))
import Linear (V3 (V3), Metric (dot, norm), (^*), normalize, Additive ((^-^)))
import SceneCommand (getMaterial, Store)
import Debug.Trace



simpleTestingShader :: Store -> Shape -> V3 Float -> V3 Float -> Ray -> V3 Float
simpleTestingShader scene hshape hitPos normal (Ray org dir rtype) = cDiffuse + cSpecular
    where
        MkMaterial ambient diffuse specular emission shininess = getShapeMaterial hshape
        plightLoc = V3 0.1 0.2 1.0
        lightDir = normalize (plightLoc ^-^ hitPos)
        cDiffuse = diffuse ^* max (normal `dot` lightDir)  0
        h = normalize(normalize (org - hitPos) + lightDir)
        cSpecular = specular ^* (max (h `dot` normal) 0 ** shininess)

getShapeMaterial:: Shape -> Material
getShapeMaterial shape = case shape of
                            {
                                Sphere _ _ mat _ _ _ -> mat;
                                Triangle _ _ _ mat _ _ _ -> mat;
                            }

test :: V3 Double
test = V3 (-0.22269252) 0.1588609 0.9618582

-- >>> norm test
-- Variable not in scope: norm :: V3 Double -> t
-- 0.9999999704599998
