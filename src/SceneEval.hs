module SceneEval where

import SceneCommand as C
import RTPrimitive
import Transform
import Control.Monad.State
import Data.Vect ( MultSemiGroup((.*.)), Vec3(Vec3) )
import Utility

defaultState :: Store
defaultState = MkStore ([],
                        [],
                        [],
                        [identity4],
                        MkMaterial (Vec3 0 0 0) (Vec3 0 0 0) (Vec3 0 0 0) 0)


tcommands :: Command
tcommands = C.Seq C.Push (C.Seq (C.Trans (Vec3 1 0 0)) (C.Sph (Vec3 0 1 0) 1))-- C.Trans (Vec3 0 1 0))

-- >>> translate3 (Vec3 0 0 0) identity4
-- Variable not in scope: translate3 :: Vec3 -> Mat4 -> t

execC :: Command -> Store -> Store
execC c = execState (evalC c)

-- >>> execC tcommands defaultState
-- MkStore ([MkSphere 1.0 (Vec3 0.0 1.0 0.0) (MkMaterial (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0) 0.0) (Mat4 (Vec4 1.0 0.0 0.0 1.0) (Vec4 0.0 1.0 0.0 0.0) (Vec4 0.0 0.0 1.0 0.0) (Vec4 0.0 0.0 0.0 1.0))],[],[],[Mat4 (Vec4 1.0 0.0 0.0 1.0) (Vec4 0.0 1.0 0.0 0.0) (Vec4 0.0 0.0 1.0 0.0) (Vec4 0.0 0.0 0.0 1.0),Mat4 (Vec4 1.0 0.0 0.0 0.0) (Vec4 0.0 1.0 0.0 0.0) (Vec4 0.0 0.0 1.0 0.0) (Vec4 0.0 0.0 0.0 1.0)],MkMaterial (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0) 0.0)

evalC :: Command -> State Store ()
evalC (C.Seq c1 c2) = do
    evalC c1
    evalC c2


evalC (C.Trans v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat .*. translate v):tail transMatStack)

evalC (C.Rot deg v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat .*. rotate (deg2rad deg) v):tail transMatStack)


evalC (C.Scale v) = do
    s <- get
    let transMatStack = C.getTransform s
    let transMat = head $ C.getTransform s
    put $ setTransform s ((transMat .*. scale v):tail transMatStack)


evalC (C.Tri  v1 v2 v3) = do
    s <- get
    put s

evalC (C.Sph v r) = do
    s <- get
    let sphlist = C.getSphereList s
    let newSphlist = MkSphere r v (C.getMaterial s) (head $ C.getTransform s):sphlist
    put $ setSphereList s newSphlist

evalC (C.PL v intensity) = do
    s <- get
    put s

evalC (C.Diffuse c) = do
    s <- get
    put s

evalC (C.Specular c) = do
    s <- get
    put s

evalC (C.Emission c) = do
    s <- get
    put s

evalC (C.Shininess intensity) = do
    s <- get
    put s

evalC End = do
    s <- get
    put s

evalC Push = do
    s <- get
    let transMatStack = C.getTransform s
    put $ setTransform s (head transMatStack:transMatStack)

evalC Pop = do
    s <- get
    let transMatStack = C.getTransform s
    put $ setTransform s (tail transMatStack)



-- eval :: Command -> State Store ()
-- eval s (C.Seq c1 c2) = do
--     eval c1
--     eval c2

