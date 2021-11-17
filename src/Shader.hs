module Shader where
import RTPrimitive (Material(MkMaterial), Shape (Sphere, Triangle))
import Linear (V3)
import SceneCommand (getMaterial, Store)




simpleTestingShader :: Store -> Shape -> V3 Float -> V3 Float -> V3 Float
simpleTestingShader scene hshape hitPos normal = ambient
    where
        MkMaterial ambient diffuse specular emission shininess = getShapeMaterial hshape

getShapeMaterial:: Shape -> Material
getShapeMaterial shape = case shape of
                            {
                                Sphere _ _ mat _ _ _ -> mat;
                                Triangle _ _ _ mat _ _ _ -> mat;
                            }