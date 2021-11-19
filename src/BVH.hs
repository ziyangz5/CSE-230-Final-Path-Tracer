module BVH where

import Linear
import RTPrimitive
import Utility (v4tov3,v3tov4, scaleMax, get3X, get3Y, get3Z, identity4, identity3, (^/^))
import Control.Monad.State
import Data.List (sortBy)
import Debug.Trace

data BBox = BBox (V3 Float) (V3 Float) deriving (Show)  --minLoc maxLoc

getBBox:: Shape->BBox
getBBox (Triangle v1 v2 v3 _ trans _ _) = BBox minLoc maxLoc
    where
        V3 v1x v1y v1z = v4tov3 (trans !* v3tov4 v1 1.0)
        V3 v2x v2y v2z = v4tov3 (trans !* v3tov4 v2 1.0)
        V3 v3x v3y v3z = v4tov3 (trans !* v3tov4 v3 1.0)
        minLoc = V3 (min v1x (min v2x v3x) - 0.001) (min v1y (min v2y v3y)- 0.001) (min v1z (min v2z v3z)- 0.001)
        maxLoc = V3 (max v1x (max v2x v3x) + 0.001) (max v1y (max v2y v3y)+ 0.001) (max v1z (max v2z v3z)+ 0.001)

getBBox (Sphere r loc _ trans _ _) = BBox minLoc maxLoc
    where
        V3 cx cy cz = v4tov3 (trans !* v3tov4 loc 1.0)
        tR = r * scaleMax trans
        minLoc = V3 (cx - tR) (cy - tR) (cz - tR)
        maxLoc = V3 (cx + tR) (cy + tR) (cz + tR)

rayInsideBBox :: Ray -> BBox -> Bool
rayInsideBBox (Ray (V3 rx ry rz) _ _) (BBox (V3 vxmi vymi vzmi) (V3 vxma vyma vzma)) =
    (rx < vxma) && (ry < vyma) && (rz < vzma) && (rx > vxmi) && (ry > vymi) && (rz > vzmi)

rayLineIntersect :: Ray -> V3 Float -> V3 Float -> ((Float,Float),(Float,Float),(Float,Float))
rayLineIntersect (Ray org dir _) minLoc maxLoc =  (tx,ty,tz)
    where
        V3 t1x t1y t1z = (minLoc ^-^ org) ^/^ dir
        V3 t2x t2y t2z = (maxLoc ^-^ org) ^/^ dir
        tx = ascending (t1x,t2x)
        ty = ascending (t1y,t2y)
        tz = ascending (t1z,t2z)

ascending :: (Float,Float) -> (Float,Float)
ascending (a,b) =  if a > b then (b,a) else (a,b)

intervalOverlap :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
intervalOverlap (t1min,t1max) (t2min,t2max) (t3min,t3max) = (cmin23 < cmax23) && (cmin123 < cmax123)
    where cmin23 = max t2min t3min
          cmax23 = min t2max t3max
          cmin123 = max t1min cmin23
          cmax123 = min t1max cmax23

rayBBoxIntersection :: Ray -> BBox -> Bool
rayBBoxIntersection ray bbox@(BBox minLoc maxLoc) =
    rayInsideBBox ray bbox || intervalOverlap tx ty tz
        where
            (tx,ty,tz) = rayLineIntersect ray minLoc maxLoc

getBBoxMinLoc :: BBox -> V3 Float
getBBoxMinLoc (BBox minLoc maxLoc) = minLoc

getBBoxMaxLoc :: BBox -> V3 Float
getBBoxMaxLoc (BBox minLoc maxLoc) = maxLoc

mergingBBox::BBox->BBox->BBox
mergingBBox (BBox (V3 v1xmi v1ymi v1zmi) (V3 v1xma v1yma v1zma)) (BBox (V3 v2xmi v2ymi v2zmi) (V3 v2xma v2yma v2zma))
    = BBox minloc maxloc
        where
            minloc = V3 (min v1xmi v2xmi) (min v1ymi v2ymi) (min v1zmi v2zmi)
            maxloc = V3 (max v1xma v2xma) (max v1yma v2yma) (max v1zma v2zma)

getBVHBBox :: BVHTree -> BBox
getBVHBBox (Leaf bbox _) = bbox
getBVHBBox (Node bbox _ _) = bbox

data BVHTree
   = Leaf BBox Shape
   | Node BBox BVHTree BVHTree
   deriving (Show)

buildBVH :: [Shape] -> State Int BVHTree
buildBVH [] = error "Empty Shape List!"
buildBVH [s] = return $ Leaf (getBBox s) s
buildBVH [s1, s2] =
    do
        left <- buildBVH [s1]
        right <- buildBVH [s2]
        return $ Node (mergingBBox (getBBox s1) (getBBox s2)) left right

buildBVH shapes =
    do
        ccoord <- get
        put $ (ccoord + 1) `mod` 3
        let getSortKey  = case ccoord of {
            0 -> get3X;1 -> get3Y;_ ->get3Z;
            }
        let (left, right) = splitAt (length sortedShapes `div` 2) sortedShapes
                where
                    sortedShapes = sortBy (\s1 s2 -> if skey s1 > skey s2  then GT else LT) shapes
                    skey = getSortKey . getBBoxMinLoc . getBBox

        leftBVH <- buildBVH left
        rightBVH <- buildBVH right
        let cbbox = mergingBBox (getBVHBBox leftBVH) (getBVHBBox rightBVH)
        return $ Node cbbox leftBVH rightBVH

getBVHClosetHit :: Ray -> BVHTree -> Maybe (Shape,V3 Float,V3 Float,Float)
getBVHClosetHit ray (Leaf bbox shape) = if rayBBoxIntersection ray bbox then getHitInfo ray shape  else Nothing

getBVHClosetHit ray (Node bbox left right) = 
    if rayBBoxIntersection ray bbox
        then
            solveBranch ray left right
        else
              Nothing
solveBranch :: Ray -> BVHTree -> BVHTree -> Maybe (Shape,V3 Float,V3 Float,Float)
solveBranch ray left right =
    case (hit1,hit2) of
        (Just hit,Nothing)->Just hit
        (Nothing,Just hit)->Just hit
        (Just hit1@(_,_,_,t1),Just hit2@(_,_,_,t2))-> if t1 < t2 then Just hit1 else Just hit2
        _->Nothing
    where
        hit1 = getBVHClosetHit ray left
        hit2 = getBVHClosetHit ray right

determineHit ::  (Shape,V3 Float,V3 Float,Float) -> (Shape,V3 Float,V3 Float,Float) -> Maybe (Shape,V3 Float,V3 Float,Float)
determineHit hit1@(_,_,_,t1) hit2@(_,_,_,t2) = if t1 < t2 then Just hit1 else Just hit2

getHitInfo :: Ray -> Shape -> Maybe (Shape,V3 Float,V3 Float,Float)
getHitInfo ray shape =  case hitResult of {Nothing -> Nothing;Just (hitPos,normal,t)-> Just (shape,hitPos,normal,t)}
    where
        hitResult = hasIntersection ray shape

-- Triangle (V3 0 0 0) (V3 0 1 0) (V3 1 0 0) dummyMat identity4 identity4 identity3,
--             Triangle (V3 4 5 4) (V3 4 4 4) (V3 5 4 4) dummyMat identity4 identity4 identity3,
--             Triangle (V3 0 (-3) 0) (V3 0 (-1) 0) (V3 (-1) 0 0) dummyMat identity4 identity4 identity3,
testShape :: [Shape]
testShape = [
            Triangle (V3 0 0 0) (V3 0 1 0) (V3 1 0 0) dummyMat identity4 identity4 identity3,
            Triangle (V3 4 5 4) (V3 4 4 4) (V3 5 4 4) dummyMat identity4 identity4 identity3,
            Triangle (V3 0 (-3) 0) (V3 0 (-1) 0) (V3 (-1) 0 0) dummyMat identity4 identity4 identity3,
            Triangle (V3 0 0 2) (V3 0 (-2) 2) (V3 1 0 1) dummyMat identity4 identity4 identity3,
            Triangle (V3 (-1) (-1) (-1)) (V3 0 1 0) (V3 1 0 0) dummyMat identity4 identity4 identity3]



bvhhitResult :: Maybe (Shape, V3 Float, V3 Float, Float)
bvhhitResult = getBVHClosetHit (Ray (V3 1 0 2) (V3 (-1) (-1) (-1)) 0) testBVH

rbtest :: Bool
rbtest = rayBBoxIntersection (Ray (V3 10 10 10) (V3 1 1 1) 0) (BBox (V3 15 15 15) (V3 50 50 50))

testBVH :: BVHTree
testBVH = evalState (buildBVH testShape) 0


-- >>> rayBBoxIntersection (Ray (V3 1 1 1) (V3 (-1) (-1) (-1)) 0) (BBox (V3 (-0.0001) (-0.0001) (-0.0001)) (V3 1.0 1.0 0.0))
-- True
--


-- >>> bvhhitResult
-- "tri int"
-- Just (Triangle (V3 0.0 0.0 2.0) (V3 0.0 (-2.0) 2.0) (V3 1.0 0.0 1.0) (MkMaterial (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 0.0) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)),V3 0.5 (-0.5) 1.5,V3 0.70710677 0.0 0.70710677,0.5)
--

-- >>> hitResult
-- Just (Triangle (V3 0.0 0.0 2.0) (V3 0.0 (-2.0) 2.0) (V3 1.0 0.0 1.0) (MkMaterial (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 0.0) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0)),V3 0.5 (-0.5) 1.5,V3 0.70710677 0.0 0.70710677)
--

