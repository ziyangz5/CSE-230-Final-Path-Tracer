module RTPrimitive where
import Data.Vect ( Vec3, Mat4 )

data Material = MkMaterial Vec3 Vec3 Vec3 Float deriving (Show)-- Diffuse, Specular, Emission, Shininess. Will be changed after path tracing (roughness)

data Sphere = MkSphere Float Vec3 Material Mat4 deriving (Show)
data Triangle = MkTri Vec3 Vec3 Vec3 Material Mat4 deriving (Show)
data PointLight = MkPL Float Vec3 deriving (Show) -- will be deprecated after path tracing
data Ray = MkRay Vec3 Vec3 deriving (Show)-- Starting point, Direction
