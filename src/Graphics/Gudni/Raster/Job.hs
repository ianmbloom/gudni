{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

module Graphics.Gudni.Raster.Job
  ( GeoReference(..)
  , RasterJobMonad(..)
  , runRasterJobMonad
  , RasterJob(..)
  , rJShapePile
  , rJTilePile
  , newRasterJob
  , resetRasterJob
  , freeRasterJob
  , tileToRasterJob
  , buildRasterJob
  , outputRasterJob
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Geometry
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ShapeInfo

import Control.Monad
import Control.Monad.State
import Control.Monad.Memo.Class

import Control.Lens
import Control.DeepSeq
import Control.Concurrent
import Control.Loop

import Foreign.Storable
import Foreign.C.Types

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M



data RasterJob = RasterJob
  { _rJShapePile :: !(Pile (Shape GeoReference))
  , _rJTilePile  :: !(Pile (Tile (Slice (Shape GeoReference))))
  } deriving (Show)
makeLenses ''RasterJob

type RasterJobMonad s m = StateT (RasterJob) m

-- | Run a RasterJobMonad and return the state.
runRasterJobMonad :: MonadIO m => RasterJob -> RasterJobMonad s m a -> m RasterJob
runRasterJobMonad job code = execStateT code job

-- | Create a new rasterJob with default allocation sizes.
newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do  initShapePile <- newPile :: IO (Pile (Shape GeoReference))
        initTilePile  <- newPile :: IO (Pile (Tile (Slice (Shape GeoReference))))
        return RasterJob
            { _rJShapePile = initShapePile
            , _rJTilePile  = initTilePile
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJShapePile
        freePile $ job ^. rJTilePile

-- | Reset pile cursors for the entire job and erase the shape map
resetRasterJob :: MonadIO m => RasterJobMonad s m ()
resetRasterJob =
    do  rJShapePile %= resetPile
        rJTilePile  %= resetPile

referenceShape :: Shape ShapeEntry -> Shape GeoReference
referenceShape (Shape info shapeEntry) = Shape info (shapeEntry ^. shapeGeoRef)

appendShapes :: MonadIO m
            => [Shape GeoReference]
            -> RasterJobMonad DisplaySpace m (Slice (Shape GeoReference))
appendShapes shapes =
       -- add the shape to the pile of shapes and return a reference to it.
       addListToPileState rJShapePile shapes

tileToRasterJob :: MonadIO m
                => Tile TileEntry
                -> RasterJobMonad DisplaySpace m ()
tileToRasterJob tile =
  do  let shapeEntries = tile ^. tileRep . tileShapes
      slice <- appendShapes $ map referenceShape shapeEntries
      let tileInfo = set tileRep slice tile
      addToPileState rJTilePile tileInfo
      return ()

buildRasterJob :: MonadIO m
                => [Tile TileEntry]
                -> m RasterJob
buildRasterJob tiles =
  do  job <- liftIO newRasterJob
      job' <- runRasterJobMonad job $
                  mapM tileToRasterJob tiles
      --liftIO $ outputRasterJob job'
      return job'

outputRasterJob :: RasterJob -> IO ()
outputRasterJob job =
  do
    putStrLn "---------------- rJShapePile ----------------------"
    print . view rJShapePile $ job
    putStrList =<< (pileToList . view rJShapePile $ job)
    putStrLn "---------------- rJTilePile -----------------------"
    putStrList =<< (pileToList . view rJTilePile $ job)

instance NFData RasterJob where
  rnf (RasterJob a b) = a `deepseq` b `deepseq` ()
