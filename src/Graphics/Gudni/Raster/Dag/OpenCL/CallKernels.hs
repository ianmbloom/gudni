{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Graphics.Gudni.Raster.Dag.OpenCL.CallKernels
  ( runTraverseDagKernel
  , runTraverseDagKernelTiles
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface
import Graphics.Gudni.Raster.Dag.OpenCL.Rasterizer
import Graphics.Gudni.Raster.Dag.OpenCL.PrepareBuffers
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.State

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Raster.OpenCL.Buffer
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.CTypeConversion
import Graphics.Gudni.Util.Util

import CLUtil
import CLUtil.KernelArgs
import CLUtil.VectorBuffers

import qualified Data.Map      as M
import qualified Data.Sequence as S
import qualified Data.Sequence ((<|),(|>))
import qualified Data.Vector   as V
import qualified Data.Vector.Storable as VS
import Data.Traversable
import Data.Foldable
import Data.Bits
import Data.Maybe
import Control.Lens.Indexed
import Linear.V4

import Control.Monad.Identity
import Control.Monad.State
import Control.Lens
import Control.Applicative
import Control.Loop

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

announceKernel :: MonadIO m => String -> m a -> m a
announceKernel name f =
  do liftIO $ putStrLn ("start " ++ name ++ "  XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
     result <- f
     liftIO $ putStrLn ("stop  " ++ name ++ "  XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
     return result

runTraverseDagKernel :: forall s token target
                     .  (  KernelArgs
                          'KernelSync
                          'NoWorkGroups
                          'UnknownWorkItems
                          'Z
                          (target -> NumWorkItems -> WorkGroup -> CL ())
                        )
                     => DagOpenCLState
                     -> BuffersInCommon s
                     -> FabricTagId
                     -> Tile
                     -> Point2 PixelSpace
                     -> Int
                     -> target
                     -> CL ()
runTraverseDagKernel rasterizer
                     bic
                     dagRoot
                     tile
                     canvasSize
                     frameCount
                     target =
    let tileWidth  = tr "tileWidth " $ fromIntegral . fromAlong Horizontal $ tile ^. widthBox
        tileHeight = tr "tileHeight" $ fromIntegral . fromAlong Vertical   $ tile ^. heightBox
    in
    announceKernel "traverseDagKernel" $
        do  randomHeap <- bufferFromVector "randomHeap      " (rasterizer ^. dagOpenCLRandomField)
            runKernel (rasterizer ^. dagOpenCLTraverseDagKernel)
                      (bic ^. bicPrimBezierHeap  )
                      (bic ^. bicPrimFacetHeap   )
                      (bic ^. bicPrimBoxHeap     )
                      (bic ^. bicPrimTagHeap     )
                      (bic ^. bicFabricTagHeap   )
                      (bic ^. bicFabricHeap      )
                      (bic ^. bicTreeRootHeap    )
                      (bic ^. bicTreeConfineHeap )
                      (bic ^. bicTreeDecoHeap    )
                      (bic ^. bicCrossingPile    )
                      (bic ^. bicPictHeap        )
                      randomHeap
                      (unRef . unFabricTagId $ dagRoot)
                      tile
                      (toCInt $ rasterizer ^. dagOpenCLDeviceSpec  . specColumnDepth)
                      (tr "canvasSize ^^^^^^^^^^^^^^^^^^^^^^^^" $ fmap (toCInt . fromIntegral) canvasSize)
                      (toCInt frameCount)
                      target
                      (Work2D tileWidth tileHeight)
                      (WorkGroup [tileWidth, 1]) :: CL ()

runTraverseDagKernelTiles :: forall s token target
                          .  (  KernelArgs
                               'KernelSync
                               'NoWorkGroups
                               'UnknownWorkItems
                               'Z
                               (target -> NumWorkItems -> WorkGroup -> CL ())
                             )
                          => DagOpenCLState
                          -> BuffersInCommon s
                          -> FabricTagId
                          -> Point2 PixelSpace
                          -> Int
                          -> target
                          -> CL ()
runTraverseDagKernelTiles rasterizer
                          bic
                          dagRoot
                          canvasSize
                          frameCount
                          target =
  let tileWidth   = toAlong Horizontal $ rasterizer ^. dagOpenCLDeviceSpec . specMaxTileSize
      tileHeight  = toAlong Vertical   $ rasterizer ^. dagOpenCLDeviceSpec . specMaxTileSize
      tileColumns = (canvasSize ^. pX `div` tileWidth ) + 1
      tileRows    = (canvasSize ^. pY `div` tileHeight) + 1
  in  numLoop 0 tileRows    $ \ y ->
          numLoop 0 tileColumns $ \ x ->
             let left = x * tileWidth
                 top  = y * tileHeight
                 right  = left + tileWidth
                 bottom = top  + tileHeight
                 tile = makeBox left top right bottom
             in  runTraverseDagKernel rasterizer
                                      bic
                                      dagRoot
                                      tile
                                      canvasSize
                                      frameCount
                                      target
