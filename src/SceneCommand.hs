module SceneCommand where
import RTPrimitive
import GHC.Show (Show)
import Linear ( M44, V3 )


data Command = 
               Tri Int Int Int
             | Sph (V3 Float) Float
             | PL (V3 Float) Float
             | Trans (V3 Float)
             | Rot Float (V3 Float)
             | Scale (V3 Float)
             | Ambient (V3 Float)
             | Diffuse (V3 Float)
             | Emission (V3 Float)
             | Specular (V3 Float)
             | Shininess Float
             | Push
             | Pop
             | Vert (V3 Float)
             | Cam (V3 Float) (V3 Float) (V3 Float) Float
             | Path String
             | Depth Int
             | Size Int Int
             | Pass
    deriving (Show)

newtype Store = MkStore ([Shape],[PointLight],[M44 Float],Material,[V3 Float],Camera,String,Int,(Int,Int)) deriving (Show)

getShapeList :: Store -> [Shape]
getShapeList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) = sl

getPointLightList :: Store -> [PointLight]
getPointLightList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) = pl

getTransform :: Store -> [M44 Float]
getTransform (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) = trans

getMaterial :: Store -> Material
getMaterial (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) = mat

getVertList :: Store -> [V3 Float]
getVertList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) = vl


setShapeList :: Store -> [Shape] -> Store
setShapeList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) nsl = MkStore (nsl, pl,trans,mat,vl,cam,path,depth,size)

setPointLightList :: Store -> [PointLight] -> Store
setPointLightList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) npll = MkStore (sl, npll,trans,mat,vl,cam,path,depth,size)

setTransform :: Store -> [M44 Float] -> Store
setTransform (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) ntrans = MkStore (sl, pl,ntrans,mat,vl,cam,path,depth,size)

setMat :: Store -> Material -> Store
setMat (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) nmat = MkStore (sl, pl,trans,nmat,vl,cam,path,depth,size)

setVertList :: Store -> [V3 Float] -> Store
setVertList (MkStore (sl, pl,trans,mat,vl,cam,path,depth,size)) nvl = MkStore (sl, pl,trans,mat,nvl,cam,path,depth,size)

setCam :: Store -> Camera -> Store
setCam (MkStore (sl, pl,trans,mat,vl,_,path,depth,size)) ncam = MkStore (sl, pl,trans,mat,vl,ncam,path,depth,size)

setPath :: Store -> String -> Store
setPath (MkStore (sl, pl,trans,mat,vl,cam,_,depth,size)) npath = MkStore (sl, pl,trans,mat,vl,cam,npath,depth,size)

setDepth :: Store -> Int -> Store
setDepth (MkStore (sl, pl,trans,mat,vl,cam,path,_,size)) ndepth = MkStore (sl, pl,trans,mat,vl,cam,path,ndepth,size)

setSize :: Store -> (Int,Int) -> Store
setSize (MkStore (sl, pl,trans,mat,vl,cam,path,depth,_)) nsize = MkStore (sl, pl,trans,mat,vl,cam,path,depth,nsize)