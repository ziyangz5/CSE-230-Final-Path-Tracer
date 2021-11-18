module Utility where
import Linear ( M33, M44, V3(V3), V4(V4) )
import Data.Word (Word64)
import Random.MWC.Primitive (Seed)
import Random.MWC.Pure
import Random.MWC.Pure (seed)


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

scaleMax :: M44 Float -> Float
scaleMax (V4 (V4 a00 a01 a02 a03)
             (V4 a10 a11 a12 a13 )
             (V4 a20 a21 a22 a23 )
             (V4 a30 a31 a32 a33 )) = max 1.0 (max a00 (max a11 a22))


deg2rad :: Float -> Float
deg2rad deg = deg * 4.0 * atan 1.0 / 180.0


rad2deg :: Float -> Float
rad2deg rad = rad * 180.0 / 3.1415926535

v4tov3Proj :: V4 Float -> V3 Float
v4tov3Proj (V4 s1 s2 s3 s4) = V3 (s1/s4) (s2/s4) (s3/s4)

v4tov3 :: V4 Float -> V3 Float
v4tov3 (V4 s1 s2 s3 s4) = V3 s1 s2 s3

v3tov4 :: V3 Float -> Float -> V4 Float
v3tov4 (V3 s1 s2 s3) s4 = V4 s1 s2 s3 s4

m4tom3 :: M44 Float -> M33 Float
m4tom3 (V4 (V4 a00 a01 a02 a03)
           (V4 a10 a11 a12 a13 )
           (V4 a20 a21 a22 a23 )
           (V4 a30 a31 a32 a33 )) =  V3 (V3 a00 a01 a02)
                                        (V3 a10 a11 a12)
                                        (V3 a20 a21 a22)

(@==@) :: Float -> Float -> Bool
t1 @==@ t2 = abs (t1-t2) < 0.000001

getRand :: Seed -> (Float,Seed)
getRand = range_random (-1,1)

get3X :: V3 Float -> Float 
get3X (V3 x y z) = x

get3Y :: V3 Float -> Float 
get3Y (V3 x y z) = y

get3Z :: V3 Float -> Float 
get3Z (V3 x y z) = z

-- >>> 3 `sm4` identity4 
-- Mat4 (Vec4 3.0 0.0 0.0 0.0) (Vec4 0.0 3.0 0.0 0.0) (Vec4 0.0 0.0 3.0 0.0) (Vec4 0.0 0.0 0.0 3.0)
