module Transform where
import Linear
    ( (!+!),
      (*!!),
      normalize,
      cross,
      M44,
      V3(..),
      V4(V4),
      Additive((^-^)) )
import Utility

translate :: V3 Float -> M44 Float
translate (V3 x y z) = V4 (V4 1 0 0 x)
                          (V4 0 1 0 y)
                          (V4 0 0 1 z)
                          (V4 0 0 0 1)

scale :: V3 Float -> M44 Float
scale (V3 x y z) = V4 (V4 x 0 0 0)
                      (V4 0 y 0 0)
                      (V4 0 0 z 0)
                      (V4 0 0 0 1)


ratMat1 :: M44 Float
ratMat1 = identity4


ratMat2 :: V3 Float -> M44 Float
ratMat2 (V3 x y z) = V4 (V4 (x*x) (x*y) (x*z) 0)
                        (V4 (x*y) (y*y) (z*y) 0)
                        (V4 (x*z) (y*z) (z*z) 0)
                        (V4 0 0 0 1)


ratMat3 :: V3 Float -> M44 Float
ratMat3 (V3 x y z) = V4 (V4 0 (-z) y 0)
                        (V4 z 0 (-x) 0)
                        (V4 (-y) x 0 0)
                        (V4 0 0 0 1)

rotate :: Float -> V3 Float -> M44 Float
rotate rad axis = lastToOne(cos rad *!! ratMat1 !+! ((1 - cos rad) *!! ratMat2 axis) !+! sin rad *!! ratMat3 axis)

getWUV :: V3 Float -> V3 Float -> V3 Float -> (V3 Float,V3 Float,V3 Float)
getWUV eye center up = (w,u,v) where w = normalize (eye ^-^ center)
                                     u = up `cross` w
                                     v = w `cross` u

-- >>>  translate (Vec3 1 2 (-1)) *. (Vec4 0 0 0 1)
-- Vec4 1.0 2.0 (-1.0) 1.0

-- >>> (rotate (deg2rad 180) (Vec3 0 1 0) .*. translate (Vec3 1 0 0)) *. Vec4 0 0 0 1
-- Vec4 (-1.0) 0.0 8.742278e-8 1.0
