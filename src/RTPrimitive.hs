module RTPrimitive where

import Linear ( M44, V3 )


data Material = MkMaterial (V3 Float) (V3 Float) (V3 Float) (V3 Float) Float deriving (Show)-- Ambient, Diffuse, Specular, Emission, Shininess. Will be changed after path tracing (roughness)

data Shape = Sphere Float (V3 Float) Material (M44 Float) (M44 Float) 
           | Triangle (V3 Float) (V3 Float) (V3 Float) Material (M44 Float) (M44 Float) deriving (Show)

data PointLight = MkPL Float (V3 Float) (M44 Float) (M44 Float) deriving (Show) -- will be deprecated after path tracing
data Ray = MkRay (V3 Float) (V3 Float) deriving (Show)-- Starting point, Direction

data Camera = Camera (V3 Float) (V3 Float) (V3 Float) (V3 Float) (V3 Float) (V3 Float) Float deriving (Show)