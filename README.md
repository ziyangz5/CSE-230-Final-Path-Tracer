# CSE230-Final-Path-Tracer

# Goal

In this project, we will implement a ray tracing system in Haskell (specifically, a [path tracer](https://en.wikipedia.org/wiki/Path_tracing)) from scratch. After finishing this project, we expect that we can render some photorealistic images using Haskell. This project will include three parts:

## IO System

A parser will be written to support reading scene files. The scene files will usually contain the shape information such as the locations of triangles surfaces and spheres, lighting parameters, and other rendering options. We will use an image library to read textures and output result images.

## Path Tracing Algorithm

The path tracing algorithm will include two parts:

### Ray-primitive Collision Detection

We will implement ray-triangle and ray-sphere collision detection algorithms. To improve the performance, we will also implement a bounding volume hierarchy (BVH) tree.

### Shading and rendering equation

We will build several data types such as triangles, spheres, rays, lights, and cameras to construct the shading algorithm.

We expect to implement the [GGX rendering equation](https://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf). We will use the Monte Carlo integration to evaluate the equation. After calculating the rendering equation, we can get the RGB value for each pixel on the image. 

If time allows, We can also implement texture mapping and make our system support texturing.

## Post-processing Algorithms

We will implement some filters such as median filter and gaussian filter to denoise the output to get better images. If time allows, some other advanced denoising algorithm may also be implemented.


## Expected Dependencies

We will not use any pre-built computer graphics libraries. We expect only to use a vector-matrix operation library and an image library used to input/output images.