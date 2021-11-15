module Utility where
import Linear ( M33, M44, V3(V3), V4(V4) )


identity3 :: M33 Float
identity3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)


identity4 :: M44 Float
identity4 = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)


lastToOne :: M44 Float -> M44 Float 
lastToOne (V4 (V4 a00 a01 a02 a03) 
              (V4 a10 a11 a12 a13 ) 
              (V4 a20 a21 a22 a23 ) 
              (V4 a30 a31 a32 a33 )) = V4 (V4 a00 a01 a02 a03) 
                                          (V4 a10 a11 a12 a13 ) 
                                          (V4 a20 a21 a22 a23 ) 
                                          (V4 a30 a31 a32 1 )

deg2rad :: Float -> Float
deg2rad deg = deg * 4.0 * atan 1.0 / 180.0


rad2deg :: Float -> Float
rad2deg rad = rad * 180.0 / 3.1415926535
 
 
-- >>> 3 `sm4` identity4 
-- Mat4 (Vec4 3.0 0.0 0.0 0.0) (Vec4 0.0 3.0 0.0 0.0) (Vec4 0.0 0.0 3.0 0.0) (Vec4 0.0 0.0 0.0 3.0)
