module SceneCommand where
import Data.Vect (Vec3, Mat4)
import RTPrimitive
import GHC.Show (Show)


data Command = Seq Command Command
             | Tri Vec3 Vec3 Vec3
             | Sph Vec3 Float
             | PL Vec3 Float
             | Trans Vec3
             | Rot Float Vec3
             | Scale Vec3
             | Diffuse Vec3
             | Emission Vec3
             | Specular Vec3
             | Shininess Float
             | Push
             | Pop
             | End

newtype Store = MkStore ([Sphere], [Triangle], [PointLight],[Mat4],Material) deriving (Show)

getSphereList :: Store -> [Sphere]
getSphereList (MkStore (sl, tl, pl,trans,mat)) = sl

getTriangleList :: Store -> [Triangle]
getTriangleList (MkStore (sl, tl, pl,trans,mat)) = tl

getPointLightList :: Store -> [PointLight]
getPointLightList (MkStore (sl, tl, pl,trans,mat)) = pl

getTransform :: Store -> [Mat4]
getTransform (MkStore (sl, tl, pl,trans,mat)) = trans

getMaterial :: Store -> Material
getMaterial (MkStore (sl, tl, pl,trans,mat)) = mat


setSphereList :: Store -> [Sphere] -> Store
setSphereList (MkStore (sl, tl, pl,trans,mat)) nsl = MkStore (nsl, tl, pl,trans,mat)

setTriangleList :: Store -> [Triangle] -> Store
setTriangleList (MkStore (sl, tl, pl,trans,mat)) ntl = MkStore (sl, ntl, pl,trans,mat)

setPointLightList :: Store -> [PointLight] -> Store
setPointLightList (MkStore (sl, tl, pl,trans,mat)) npll = MkStore (sl, tl, npll,trans,mat)

setTransform :: Store -> [Mat4] -> Store
setTransform (MkStore (sl, tl, pl,trans,mat)) ntrans = MkStore (sl, tl, pl,ntrans,mat)

setMat :: Store -> Material -> Store
setMat (MkStore (sl, tl, pl,trans,mat)) nmat = MkStore (sl, tl, pl,trans,nmat)