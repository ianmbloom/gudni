# Welcome to Gudni

Gudni is a fast 2d rasterizer built specifically for the Haskell ecosystem.

It is named for the late Icelandic painter Georg Guðni Hauksson.

Here are some of the features of the system:

* GPU accelerated rasterization.
* High quality anti-aliasing of objects with large amount of subpixel detail.
* Stochastic anti-aliasing (experimental) is an attempt to add a random factor to mask spatial aliasing and "naturalize" an image.
* Support for transparency.
* Support for additive and subtractive shape geometry on the fly.
* Rudimentary image texturing support.

The goal is to create an engine that can create a realtime output for figures with the complexity of a normal interface (~10,000 shapes) and slower but perfect outputs for highly complex images  with the complexity of artwork. (1M+ shapes). Getting there will require moving more of the current shape pipeline onto the GPU, which is in the roadmap.

## Code Guide

The purpose of this library is to convert a vector description of a figure to a raster image. Because Gudni uses a GPU via OpenCL for part of this computation the library must just-in-time compiles its OpenCL kernels prior doing any work and must interact with the GPU inside of an IO and CLState monad. Unfortunately because of this there is no way to use the rasterizer as a pure function. To abstract the details of this interaction away from someone using the library we use a callback style API. Users initially call the function runApplication and pass that function a data structure which represents the state of their application and is an instance of typeclass Model. The typeclass defines functions which can modify this state and provide the rasterizer with information based on the state, including a constructScene which provides the rasterizer with all of the vector geometry and metadata that describes an individual image. The specifics of these functions are illustrated in the following examples.

### Example 1: Simple Square.
The file examples/Square.hs provides the minimal demonstration of the library API. The program defines a datatype SquareState which happens to contain values for the scale and rotation of a square.
We declare SquareState to be an instance of typeclass Model. The Model instance defines the functions which are our main API for creating Gudni applications.
Importantly we define:

updateModel -- which takes the current elapsed time and a list of inputs and produces an updated state, in this case a new SquareState.

constructFigure -- which takes the current state (and a status string) and constructs a Scene for each frame of the image sequence we are creating.

A Scene contains a ShapeTree. A ShapeTree is a tree whose leaves are CompoundTrees, it defines the order that each leaf is layered and specifies a texture for each leaf. CompoundTree is a tree of Outlines, it specifies how outlines are added to and subtracted to one another. And Outline is a sequence of coordinates that define a quadratic Bezier loop.

Both ShapeTree and CompoundTrees are just specializations of STree and both also have transformation nodes define transformations that are applied to all of the nodes children.

The file /Util/Draw.hs has many combinators for creating ShapeTrees.
Each combinator is actually a member of a typeclass that has instances depending on what type of shape you want to define. Importantly there are instances that define basic shapes as either naked CompoundTrees or Glyph (CompoundTree). A Glyph in gudni is a wrapper around any representation of the shape that defines the bounding box of the shape.

So for example the statement:

    constructScene state status =
        return .                          -- Push the scene into the FontMonad
        Scene (light . greenish $ blue) . -- Wrap the ShapeTree in a scene with background color
        Just .                             
        tTranslate (Point2 100 100) .     -- Translate the child ShapeTree
        tScale  (state ^. stateScale) .   -- Scale the child ShapeTree based on the current state.
        tRotate (state ^. stateAngle) .   -- Rotate the child ShapeTree based on the current state.
        solid yellow $                    -- Create a leaf of the ShapeTree and fill the contained CompoundTree with a solid color.
        unitSquare                        -- Create new compoundTree with just one leaf that is the outline of a unit square.

### Example 2: Plot

This program uses the turtle plotting functions defined in Graphics.Gudni.Util.Plot to make interesting shapes.

### Example 3: Paragraph

A demostration of Gudni's bare bones layout capabilities.

### Benchmarks Program

The folder benchmarks contains two programs that use Gudni as a library.
GudniBenchmarks.hs has a series of drawings designed to test performance and find bugs. Run this with

cabal new-run gudni-benchmarks

keyboard responds to: "a", "w", "s", "d" translate drawing, "r", "t" rotate, "[" "]" scale, left arrow, right arrow, select drawing, spacebar run/pause animation

Reports as to the performance of this program on your system are appreciated.

### Trace Visualizer

GudniTraceVisualizer can read an output extracted from the opencl kernel and draw a diagram of the trace. I've been using this to find edge cases. (It's usually broken until it's needed again.)

cabal new-run gudni-trace-visualizer

## How the Code is Organized

The source folders are organized as such:

* /Figure -- source related to creating ShapeTrees
* /Interface -- source related to interacting with SDL2 or other frontend libraries.
* /Layout -- combinators for creating ShapeTrees as well a bare bones system for defining bounding boxes for shapes and placing them adjacent to one another.
* /OpenCL -- source related to GPU specific code.
* /Raster -- source related to prepping shapes to be rasterized on the gpu kernel.
* /Util -- other stuff

## Build Notes

Modifying OpenCL Kernels:

Kernels.cl currently includes all opencl code. This is referenced by the EmbeddedOpenCLSource.hs
On some build systems changing to Kernels.cl won't trigger a recompile of EmbeddedOpenCLSource
so a small change to EmbeddedOpenCLSource (such as adding whitespace) is needed to get the new OpenCL code changes to be reembedded.

CLUtil:

I'm using the most recent version of CLUtil from github https://github.com/acowley/CLUtil

OpenCL library:

To get it to compile on MacOSX Sierra I changed the following in my local OpenCL.cabal file:

```
  if os(darwin)
    cpp-options: -DCALLCONV=ccall -D__nullable= -DGL_SILENCE_DEPRECATION
    cc-options: "-U__BLOCKS__"
    Frameworks:  OpenCL
```

SDL2:
Various different methods of installing SDL2 on Mac put the includes in all kinds of different places. Try building the library from the source with "make install" or adjust the extra-include-dirs to find the SDL_headers.
