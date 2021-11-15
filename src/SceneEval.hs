module SceneEval where
import SceneCommand as C
import RTPrimitive
import Transform ( scale, rotate, translate, getWUV )
import Control.Monad.State
import Linear ( (!*!), inv44, V3(V3) )
import Utility

defaultState :: Store
defaultState = MkStore ([],
                        [],
                        [identity4],
                        MkMaterial (V3 0 0 0)  (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) 0,
                        [],
                        Camera (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) (V3 0 0 0) 0,
                        "./output.png",2,(100,100))


tcommands :: [Command]
tcommands = [C.Push,C.Trans (V3 1 0 0),C.Sph (V3 0 1 0) 1,C.Pop]

-- >>> translate3 (Vec3 0 0 0) identity4
-- Variable not in scope: translate3 :: Vec3 -> Mat4 -> t

execC :: [Command] -> Store -> Store
execC cs = execState (evalCL cs)

-- >>> execC tcommands defaultState
-- MkStore ([Sphere 1.0 (V3 0.0 1.0 0.0) (MkMaterial (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 0.0) (V4 (V4 1.0 0.0 0.0 1.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 (-1.0)) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0))],[],[V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)],MkMaterial (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 0.0,[],Camera (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 0.0,"./output.png",2,(100,100))

evalCL :: [Command] -> State Store ()
evalCL (c:cs) = do
    evalC c
    evalCL cs

evalCL [] = do
    s <- get
    put s

evalC :: Command -> State Store ()
evalC (C.Trans v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat !*! translate v):tail transMatStack)

evalC (C.Rot deg v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat !*! rotate (deg2rad deg) v):tail transMatStack)


evalC (C.Scale v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat !*! scale v):tail transMatStack)


evalC (C.Tri  v1 v2 v3) = do
    s <- get
    let triList = C.getShapeList s
    let vertList = C.getVertList s
    let newTrilist = Triangle (vertList!!v1) (vertList!!v2) (vertList!!v3) (C.getMaterial s) (head $ C.getTransform s) (inv44 $ head $ C.getTransform s) : triList
    put $ C.setShapeList s newTrilist

evalC (C.Sph v r) = do
    s <- get
    let sphlist = C.getShapeList s
    let newSphlist = Sphere r v (C.getMaterial s) (head $ C.getTransform s) (inv44 $ head $ C.getTransform s):sphlist
    put $ C.setShapeList s newSphlist

evalC (C.PL v intensity) = do
    s <- get
    let plList = C.getPointLightList s
    let newPlList = MkPL intensity v (head $ C.getTransform s) (inv44 $ head $ C.getTransform s) : plList
    put $ C.setPointLightList s newPlList

evalC (C.Ambient nambi) = do
    s <- get
    case C.getMaterial s of 
    { 
        MkMaterial _ dif spec emi sh -> put $ C.setMat s (MkMaterial nambi dif spec emi sh)
    }

evalC (C.Diffuse ndif) = do
    s <- get
    case C.getMaterial s of 
    { 
        MkMaterial ambi _ spec emi sh -> put $ C.setMat s (MkMaterial ambi ndif spec emi sh)
    }

evalC (C.Specular nspec) = do
    s <- get
    case C.getMaterial s of 
    { 
        MkMaterial ambi dif _ emi sh -> put $ C.setMat s (MkMaterial ambi dif nspec emi sh)
    }

evalC (C.Emission nemi) = do
    s <- get
    case C.getMaterial s of 
    { 
        MkMaterial ambi dif spec _ sh -> put $ C.setMat s (MkMaterial ambi dif spec nemi sh)
    }

evalC (C.Shininess nsh) = do
    s <- get
    case C.getMaterial s of 
    { 
        MkMaterial ambi dif spec emi _ -> put $ C.setMat s (MkMaterial ambi dif spec emi nsh)
    }

evalC (Vert v)= do
    s <- get
    let vertList = C.getVertList s
    put $ C.setVertList s (vertList ++ [v])

evalC C.Push = do
    s <- get
    let transMatStack = C.getTransform s
    put $ C.setTransform s (head transMatStack:transMatStack)

evalC C.Pop = do
    s <- get
    let transMatStack = C.getTransform s
    put $ C.setTransform s (tail transMatStack)

evalC (C.Cam eye center up fovy) = do
    s <- get
    let (w,u,v) = getWUV eye center up
    put $ C.setCam s (Camera eye center up w u v fovy)

evalC (C.Path p) = do
    s <- get
    put $ C.setPath s p


evalC (C.Depth depth) = do
    s <- get
    put $ C.setDepth s depth


evalC (C.Size w h) = do
    s <- get
    put $ C.setSize s (w,h)

evalC C.Pass = do
    s <- get
    put s





