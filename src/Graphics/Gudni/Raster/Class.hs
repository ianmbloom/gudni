


module Graphics.Gudni.Raster.Class
  ( Rasterizer(..)
  )
where

class Rasterizer r where
    collectPictureMemory :: PictureMap -> r -> IO r
    drawFrame ::
    
