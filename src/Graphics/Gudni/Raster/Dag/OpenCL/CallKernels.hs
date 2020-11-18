{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Graphics.Gudni.Raster.Dag.OpenCL.CallKernels
  (
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface
import Graphics.Gudni.Raster.Dag.OpenCL.RasterState
import Graphics.Gudni.Raster.Dag.OpenCL.PrepareBuffers
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.State

import Graphics.Gudni.Util.Debug
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
                     => DagState token s
                     -> BuffersInCommon s
                     -> FabricTagId
                     -> Tile
                     -> target
                     -> CL ()
runTraverseDagKernel state
                     bic
                     dagRoot
                     tile
                     target =
    let tileWidth  = fromIntegral . fromAlong Horizontal $ state ^. dagTile . widthBox
        tileHeight = fromIntegral . fromAlong Vertical   $ state ^. dagTile . heightBox
    in
    announceKernel "traverseDagKernel" $
    runKernel (state ^. dagRasterState . rasterTraverseDagKernel)
              (bic ^. bicPrimBezierHeap  )
              (bic ^. bicPrimFacetHeap   )
              (bic ^. bicPrimBoxHeap     )
              (bic ^. bicPrimTagHeap     )
              (bic ^. bicFabricTagHeap   )
              (bic ^. bicFabricHeap      )
              (bic ^. bicTreeConfineHeap )
              (bic ^. bicTreeDecoHeap    )
              (bic ^. bicCrossingPile    )
              (bic ^. bicPictHeap        )
              (bic ^. bicRandomHeap      )
              (unRef . unFabricTagId $ dagRoot)
              tile
              (toCInt $ state ^. dagRasterState . rasterDeviceSpec . specColumnDepth)
              (toCInt . fromIntegral <$> state ^. dagBitmapSize)
              (toCInt $ state ^. dagFrameCount)
              target
              (Work2D tileWidth tileHeight)
              (WorkGroup [tileWidth, tileHeight]) :: CL ()
