import Test.Hspec
import Utility
import Transform
import Linear ( V3(V3), V4(V4),M44,norm, inv33, inv44 )
import Data.Maybe (isJust)
import RTPrimitive
import Linear.Matrix (transpose)
import BVH (rayBBoxIntersection, BBox (BBox), buildBVH, testShape, BVHTree (Leaf), getBVHClosetHit)
import Control.Monad.State
import BVH (testShape2)
import Shader (getGGXBRDF, getGGXPdf)


matEq :: M44 Float -> M44 Float -> Bool
matEq  (V4 (V4 a00 a01 a02 a03)
           (V4 a10 a11 a12 a13 )
           (V4 a20 a21 a22 a23 )
           (V4 a30 a31 a32 a33 ))

       (V4(V4 b00 b01 b02 b03)
           (V4 b10 b11 b12 b13 )
           (V4 b20 b21 b22 b23 )
           (V4 b30 b31 b32 b33 ))
        = (a00 @==@ b00) && (a01 @==@ b01) && (a02 @==@ b02) && (a03 @==@ b03) &&
          (a10 @==@ b10) && (a11 @==@ b11) && (a12 @==@ b12) && (a13 @==@ b13) &&
          (a20 @==@ b20) && (a21 @==@ b21) && (a22 @==@ b22) && (a23 @==@ b23) &&
          (a30 @==@ b30) && (a31 @==@ b31) && (a32 @==@ b32) && (a33 @==@ b33)

v3Eq :: V3 Float -> V3 Float -> Bool
v3Eq (V3 s1 s2 s3) (V3 r1 r2 r3)  = (s1 @==@ r1) && (s2 @==@ r2)&& (s3 @==@ r3)

v4Eq :: V4 Float -> V4 Float -> Bool
v4Eq (V4 s1 s2 s3 s4) (V4 r1 r2 r3 r4)  = (s1 @==@ r1) && (s2 @==@ r2)&& (s3 @==@ r3)&& (s4 @==@ r4)

main :: IO ()
main = hspec $ do
    describe "Utility.@==@" $ do
        it "equal" $ do
            1.00001 @==@ 1.000015 `shouldBe` (True :: Bool)
        it "not equal" $ do
            1.00001 @==@ 1.00002 `shouldBe` (False :: Bool)

    describe "Utility.deg2rad" $ do
        it "test1" $ do
            deg2rad 54.34 @==@ 0.948412 `shouldBe` (True :: Bool)
        it "test2" $ do
            deg2rad 189.88 @==@ 3.3140314 `shouldBe` (True :: Bool)
        it "test3" $ do
            deg2rad 0.0 @==@ 0.0 `shouldBe` (True :: Bool)

    describe "Utility.rad2deg" $ do
        it "test1" $ do
            rad2deg 0.948412 @==@  54.34`shouldBe` (True :: Bool)
        it "test2" $ do
            rad2deg 3.3140314 @==@ 189.88 `shouldBe` (True :: Bool)
        it "test3" $ do
            rad2deg 0.0 @==@ 0.0 `shouldBe` (True :: Bool)

    describe "Transform.translate" $ do
        it "test1" $ do
            translate (V3 1 2 (-1)) `shouldBe` V4 (V4 1.0 0.0 0.0 1.0) (V4 0.0 1.0 0.0 2.0) (V4 0.0 0.0 1.0 (-1.0)) (V4 0.0 0.0 0.0 1.0)
        it "test2" $ do
            translate (V3 5 9 (-3)) `shouldBe` V4 (V4 1.0 0.0 0.0 5.0) (V4 0.0 1.0 0.0 9.0) (V4 0.0 0.0 1.0 (-3.0)) (V4 0.0 0.0 0.0 1.0)
        it "test3" $ do
            translate (V3 0 0 0) `shouldBe` V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)

    describe "Transform.scale" $ do
        it "test1" $ do
            scale  (V3 1 2 (-1)) `shouldBe` V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 2.0 0.0 0.0) (V4 0.0 0.0 (-1.0) 0.0) (V4 0.0 0.0 0.0 1.0)
        it "test2" $ do
            scale (V3 5 9 (-3)) `shouldBe` V4 (V4 5.0 0.0 0.0 0.0) (V4 0.0 9.0 0.0 0.0) (V4 0.0 0.0 (-3.0) 0.0) (V4 0.0 0.0 0.0 1.0)
        it "test3" $ do
            scale (V3 1 1 1) `shouldBe` V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)

    describe "Transform.rotate" $ do
        it "norm test" $ do
            norm (Transform.rotate (deg2rad 180) (V3 0 1 0)) `shouldBe` V4 1.0 1.0 1.0 1.0
        it "sym test" $ do
            norm (Transform.rotate (deg2rad 30) (V3 0 1 0)) `shouldBe` norm (Transform.rotate (deg2rad 330) (V3 0 1 0))
        it "example test" $ do
            matEq (rotate (deg2rad 30) (V3 1 1 0)) (V4 (V4 1.0 0.133974 0.5 0.0) (V4 0.133974 1.0 (-0.5) 0.0) (V4 (-0.5) 0.5 0.86602 0.0) (V4 0.0 0.0 0.0 1.0))`shouldBe` (True :: Bool)

    describe "Transform.getWUV" $ do
        it "norm test" $ do
            let (w,u,v) = getWUV (V3 0 2 15) (V3 0 (-1) 0) (V3 0 1 0)
            let result = (norm w @==@ 1) && (norm u @==@ 1) && (norm v @==@ 1)
            result `shouldBe` (True :: Bool)
        it "example test" $ do
            let (w,u,v) = getWUV (V3 0 2 3) (V3 2 (-1) 0) (V3 1 1 0)
            let result = v3Eq w (V3 (-0.426401) 0.63960 0.63960) && v3Eq u (V3 0.457495 (-0.457495) 0.762492) && v3Eq v (V3 0.78030 0.617743 (-0.097538))
            result `shouldBe` (True :: Bool)

    describe "RTPrimitive.hasIntersection" $ do
        it "tcollision test1" $ do
            isJust (hasIntersection (Ray (V3 0 0 0) (V3 1 0 1) 0)  (Triangle (V3 0 (-2) 4) (V3 3 2 3)  (V3 4 (-2) 0)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 1 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (True :: Bool)
        it "tcollision test2" $ do
            isJust (hasIntersection (Ray (V3 0 0 0) (V3 1 0 1) 0)  (Triangle (V3 0 (-2) 4) (V3 3 2 3)  (V3 4 (-2) 0)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 10 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (False  :: Bool)
        it "tcollision test3" $ do
            isJust (hasIntersection (Ray (V3 0 0 0.1) (V3 1 0 1) 0)  (Triangle (V3 0 (-2) 4) (V3 3 2 3)  (V3 4 (-2) 0)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 1 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (True :: Bool)
        it "tcollision test4" $ do
            isJust (hasIntersection (Ray (V3 5 0 0) (V3 1 0 1) 0)  (Triangle (V3 0 (-2) 4) (V3 3 2 3)  (V3 4 (-2) 0)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 3 10 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (False  :: Bool)

        it "scollision test1" $ do
            isJust (hasIntersection (Ray (V3 0 0 0) (V3 0 1 0) 0)  (Sphere  10.0 (V3 0 (-2) 4)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 1 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (True :: Bool)
        it "scollision test2" $ do
            isJust (hasIntersection (Ray (V3 0 0 0) (V3 1 0 1) 0)  (Sphere  1.0 (V3 0 (-2) 4)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 10 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (False  :: Bool)
        it "scollision test3" $ do
            isJust (hasIntersection (Ray (V3 0 0 0.1) (V3 1 0 1) 0)  (Sphere  20.0 (V3 0 (-2) 4)  dummyMat (translate (V3 0 1 0))  (inv44 (translate (V3 0 1 0))) (transpose $ inv33 (m4tom3 $ translate (V3 0 1 0))))) `shouldBe` (True :: Bool)

    describe "BVH.rayBBoxIntersection" $ do
        it "test1" $ do
            rayBBoxIntersection (Ray (V3 1 1 1) (V3 (-1) (-1) (-1)) 0) (BBox (V3 (-0.0001) (-0.0001) (-0.0001)) (V3 1.0 1.0 0.0)) `shouldBe` (True :: Bool)
        it "test2" $ do
            rayBBoxIntersection (Ray (V3 1 (-1) 5) (V3 (-1) (-1) (-1)) 0) (BBox (V3 (-0.0001) (-0.0001) (-0.0001)) (V3 1.0 1.0 0.0)) `shouldBe` (False :: Bool)


    describe "BVH.getBVHClosetHit" $ do
        let testBVH = evalState (buildBVH testShape) 0
        it "test1" $ do
            isJust (getBVHClosetHit (Ray (V3 1 0 2) (V3 (-1) (-1) (-1)) 0) testBVH) `shouldBe` (True :: Bool)
        it "test2" $ do
            isJust (getBVHClosetHit (Ray (V3 5 0 12) (V3 (-10) (-1) (-1)) 0) testBVH) `shouldBe` (False  :: Bool)

    describe "Shader.getGGXBRDF" $ do
        it "test1" $ do
            v3Eq (getGGXBRDF (V3 0.5 0 1)  (V3 0.1 0.1 0.1) 0.25 (V3 0 1 0) (V3 1 1 0) (V3 0 1 1) 0.1) (V3 0.16269 0.0035381 0.32184) `shouldBe` (True  :: Bool)
        it "test2" $ do
            v3Eq (getGGXBRDF (V3 0.5 0 1)  (V3 0.1 0.1 0.1) 0.25 (V3 0 (-1) 0) (V3 1 1 0) (V3 0 1 1) 0.1) (V3 0 0 0) `shouldBe` (True  :: Bool)

    describe "Shader.getGGXPdf" $ do
        it "test" $ do
            getGGXPdf (V3 0.1 0 1)  (V3 0.1 0.1 0.1) (V3 0 1 0) (V3 0 1 0) 0.3 0.25 @==@  0.0278 `shouldBe` (True  :: Bool)