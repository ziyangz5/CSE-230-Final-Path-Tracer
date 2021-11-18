module BVH where

import Linear
import RTPrimitive
import Utility (v4tov3,v3tov4, scaleMax, get3X, get3Y, get3Z)
import Control.Monad.State


data BBox = BBox (V3 Float) (V3 Float) deriving (Show)  --minLoc maxLoc

getBBox:: Shape->BBox
getBBox (Triangle v1 v2 v3 _ trans _ _) = BBox minLoc maxLoc
    where
        V3 v1x v1y v1z = v4tov3 (trans !* v3tov4 v1 1.0)
        V3 v2x v2y v2z = v4tov3 (trans !* v3tov4 v2 1.0)
        V3 v3x v3y v3z = v4tov3 (trans !* v3tov4 v3 1.0)
        minLoc = V3 (min v1x (min v2x v3x)) (min v1y (min v2y v3y)) (min v1z (min v2z v3z))
        maxLoc = V3 (max v1x (max v2x v3x)) (max v1y (max v2y v3y)) (max v1z (max v2z v3z))

getBBox (Sphere r loc _ trans _ _) = BBox minLoc maxLoc
    where
        V3 cx cy cz = v4tov3 (trans !* v3tov4 loc 1.0)
        tR = r * scaleMax trans
        minLoc = V3 (cx - tR) (cy - tR) (cz - tR)
        maxLoc = V3 (cx + tR) (cy + tR) (cz + tR)

mergingBBOX::BBox->BBox->BBox
mergingBBOX (BBox (V3 v1xmi v1ymi v1zmi) (V3 v1xma v1yma v1zma)) (BBox (V3 v2xmi v2ymi v2zmi) (V3 v2xma v2yma v2zma))
    = BBox minloc maxloc
        where
            minloc = V3 (min v1xmi v2xmi) (min v1ymi v2ymi) (min v1zmi v2zmi)
            maxloc = V3 (max v1xma v2xma) (max v1yma v2yma) (max v1zma v2zma)


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
        return $ Node (mergingBBOX (getBBox s1) (getBBox s2)) left right

buildBVH shapes = 
    do
        ccoord <- get
        put $ (ccoord + 1) `mod` 3
        let getSortKey  = case ccoord of {0 -> get3X;1 -> get3Y;_ ->get3Z;}
        error ""
    
