module Transform where
import Data.Vect
import Utility

translate :: Vec3 -> Mat4
translate (Vec3 x y z) = Mat4 (Vec4 1 0 0 x)
                              (Vec4 0 1 0 y)
                              (Vec4 0 0 1 z)
                              (Vec4 0 0 0 1)

scale :: Vec3 -> Mat4
scale (Vec3 x y z) = Mat4 (Vec4 x 0 0 0)
                          (Vec4 0 y 0 0)
                          (Vec4 0 0 z 0)
                          (Vec4 0 0 0 1)


ratMat1 :: Mat4
ratMat1 = identity4


ratMat2 :: Vec3 -> Mat4
ratMat2 (Vec3 x y z) = Mat4 (Vec4 (x*x) (x*y) (x*z) 0)
                            (Vec4 (x*y) (y*y) (z*y) 0)
                            (Vec4 (x*z) (y*z) (z*z) 0)
                            (Vec4 0 0 0 1)


ratMat3 :: Vec3 -> Mat4
ratMat3 (Vec3 x y z) = Mat4 (Vec4 0 (-z) y 0)
                            (Vec4 z 0 (-x) 0)
                            (Vec4 (-y) x 0 0)
                            (Vec4 0 0 0 1)

rotate :: Float -> Vec3 -> Mat4
rotate rad axis = lastToOne(cos rad `sm4` ratMat1 &+ ((1 - cos rad) `sm4` ratMat2 axis) &+ sin rad `sm4` ratMat3 axis)

getWUV :: Vec3 -> Vec3 -> Vec3 -> (Vec3,Vec3,Vec3)
getWUV eye center up = (w,u,v) where w = normalize (eye &- center)
                                     u = up &^ w
                                     v = w &^ u

-- >>>  translate (Vec3 1 2 (-1)) *. (Vec4 0 0 0 1)
-- Vec4 1.0 2.0 (-1.0) 1.0

-- >>> (rotate (deg2rad 180) (Vec3 0 1 0) .*. translate (Vec3 1 0 0)) *. Vec4 0 0 0 1
-- Vec4 (-1.0) 0.0 8.742278e-8 1.0
