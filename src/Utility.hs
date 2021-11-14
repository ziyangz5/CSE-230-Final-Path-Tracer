module Utility where
import Data.Vect

identity3 :: Mat3
identity3 = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)


identity4 :: Mat4
identity4 = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)

sm4 :: Float -> Mat4 -> Mat4 
sm4 s (Mat4 v1 v2 v3 v4) = Mat4 (v1 &* s) (v2 &* s) (v3 &* s) (v4 &* s)

sm3 :: Float -> Mat3 -> Mat3
sm3 s (Mat3 v1 v2 v3) = Mat3 (v1 &* s) (v2 &* s) (v3 &* s)

lastToOne :: Mat4 -> Mat4 
lastToOne (Mat4 (Vec4 a00 a01 a02 a03) 
                (Vec4 a10 a11 a12 a13 ) 
                (Vec4 a20 a21 a22 a23 ) 
                (Vec4 a30 a31 a32 a33 )) = Mat4 (Vec4 a00 a01 a02 a03) 
                                                (Vec4 a10 a11 a12 a13 ) 
                                                (Vec4 a20 a21 a22 a23 ) 
                                                (Vec4 a30 a31 a32 1 )

deg2rad :: Float -> Float
deg2rad deg = deg * 4.0 * atan 1.0 / 180.0


rad2deg :: Float -> Float
rad2deg rad = rad * 180.0 / 3.1415926535
 
 
-- >>> 3 `sm4` identity4 
-- Mat4 (Vec4 3.0 0.0 0.0 0.0) (Vec4 0.0 3.0 0.0 0.0) (Vec4 0.0 0.0 3.0 0.0) (Vec4 0.0 0.0 0.0 3.0)
