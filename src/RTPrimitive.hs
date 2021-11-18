module RTPrimitive where

import Linear
    ( V3(..),
      M44,
      M33,
      (!*),
      inv33,
      inv44,
      transpose,
      normalize,
      cross,
      (^*),
      Metric(dot),
      V4(V4), Additive ((^-^)), (*^) )
import Utility (v4tov3, identity4, v3tov4, m4tom3)
import Transform (translate)


data Material = MkMaterial (V3 Float) (V3 Float) (V3 Float) (V3 Float) Float deriving (Show)-- Ambient, Diffuse, Specular, Emission, Shininess. Will be changed after path tracing (roughness)

data Shape = Sphere Float (V3 Float) Material (M44 Float) (M44 Float) (M33 Float)
           | Triangle (V3 Float) (V3 Float) (V3 Float) Material (M44 Float) (M44 Float) (M33 Float) deriving (Show)

data Light = PointLight (V3 Float) (V3 Float) (M44 Float) (M44 Float)  -- Loc/direction, color
           | DirLight  (V3 Float) (V3 Float) (M44 Float) (M44 Float)  deriving (Show) -- will be deprecated after path tracing
data Ray = Ray (V3 Float) (V3 Float) Int deriving (Show)-- Starting point, Direction

data Camera = Camera (V3 Float) (V3 Float) (V3 Float) (V3 Float) (V3 Float) (V3 Float) Float Float deriving (Show) --eye center up w u v fovx fovy


hasIntersection:: Ray->Shape->Maybe (V3 Float,V3 Float, Float)
hasIntersection (Ray (V3 o1 o2 o3) (V3 d1 d2 d3) rtype) (Sphere r loc _ trans invTrans invTT) =
    if collided then Just (hitPos,normalize(invTT !* normalize norm),t) else Nothing
        where
            (collided,t) = sphereIntersect (Ray torg tdir rtype) r loc
            torg = v4tov3 $ invTrans !* V4 o1 o2 o3 1.0
            tdir = v4tov3 $ invTrans !* V4 d1 d2 d3 0.0
            hit = torg + tdir ^* t
            hitPos = v4tov3 (trans !* v3tov4 hit 1.0)
            norm = normalize(hit - loc)

hasIntersection (Ray (V3 o1 o2 o3) (V3 d1 d2 d3) rtype) (Triangle v1 v2 v3 _ trans invTrans invTT) =
    if collided then Just (hitPos,normalize(invTT !* normalize norm),t) else Nothing
        where
            (collided,t,norm) = triIntersect (Ray torg tdir rtype) v1 v2 v3
            torg = v4tov3 $ invTrans !* V4 o1 o2 o3 1.0
            tdir = v4tov3 $ invTrans !* V4 d1 d2 d3 0.0
            hitPos = v4tov3 (trans !* v3tov4 (torg + tdir ^* t) 1.0)


sphereIntersect:: Ray -> Float -> V3 Float -> (Bool, Float)
sphereIntersect (Ray org dir _) r loc = (t>0,t)
                                        where
                                            a = dir `dot` dir
                                            b = (2 *^ dir) `dot` (org ^-^ loc)
                                            c = (org ^-^ loc) `dot` (org ^-^  loc) - r * r
                                            delta = b * b - 4 * a * c;
                                            t1 = (-b + sqrt delta) / (2 * a)
                                            t2 = (-b - sqrt delta) / (2 * a)
                                            t' = min t1 t2
                                            t = if t' < 0 then max t1 t2 else t'
                                                --dot((rayTrans.origin - sphere.center), (rayTrans.origin - sphere.center)) - sphere.r * sphere.r;

-- formula from http://graphics.stanford.edu/courses/cs348b-17-spring-content/lectures/02_rt1/02_rt1_slides.pdf
triIntersect :: Ray -> V3 Float -> V3 Float -> V3 Float-> (Bool, Float, V3 Float)
triIntersect (Ray org dir _) v1 v2 v3 = ((beta >= 0.0) && (gamma >= 0.0) && (beta+gamma <= 1) && (t < 999) && (t > 0.001),t,n)
                                        where alpha = 1 - beta - gamma
                                              gamma = frac * s2 `dot` dir
                                              beta = frac * s1 `dot` s
                                              frac = 1 / (s1 `dot` e1)
                                              s1 = cross dir e2
                                              s2 = cross s   e1
                                              s  = org - v1
                                              e2 = v3 - v1
                                              e1 = v2 - v1
                                              t = frac * s2 `dot` e2
                                              n = cross e1 e2


dummyMat :: Material
dummyMat = MkMaterial (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) 0.0

test :: Maybe (V3 Float, V3 Float, Float)
test = hasIntersection (Ray (V3 0 0 0) (V3 1 0 1) 0)  (Triangle (V3 0 (-2) 4) (V3 3 2 3)  (V3 4 (-2) 0)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 1 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))

-- >>> test
-- Just (V3 2.25 0.0 2.25,V3 (-0.6666667) 0.33333334 (-0.6666667))


