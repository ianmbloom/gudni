{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Raster.Job
  ( GeoReference(..)
  , RasterJobMonad(..)
  , runRasterJobMonad
  , RasterJobInput(..)
  , rjiBackgroundColor
  , rjiTileTree
  , RasterJob(..)
  , rJShapePile
  , rJGroupPile
  , rJTilePile
  , rJBackgroundColor
  , rJShapeMap
  , ShapeId(..)
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

type ShapeId = Reference Shape

data TileInfo = TileInfo
  -- | Pixel boundaries of tile.
  { _tiBox    :: !(Box IntSpace)
  -- | Range of shapeIds in the geometry heap.
  , _tiShapes :: !(Slice (Shaper GeoReference))
  -- | Logarithmic horizontal depth.
  , _tiHDepth :: !(CInt)
  -- | Logarithmic vertical depth.
  , _tiVDepth :: !(CInt)
  } deriving (Show)

data RasterJobInput = RasterJobInput
  { _rjiBackgroundColor :: Color
  , _rjiShapes          :: [ShapeHeader]
  , _rjiTileTree        :: TileTree
  }
makeLenses ''RasterJobInput

data RasterJob = RasterJob
  { _rJShapePile       :: !(Pile Shape)
  , _rJTilePile        :: !(Pile TileInfo)
  , _rJGroupPile       :: !(Pile ShapeHeader)
  , _rJBackgroundColor :: !Color
  , _rJShapeMap        :: M.Map PrimId ShapeId
  } deriving (Show)
makeLenses ''RasterJob

type RasterJobMonad s m = StateT (RasterJob) m


-- | Run a RasterJobMonad and return the state.
runRasterJobMonad :: MonadIO m => RasterJob -> RasterJobMonad s m a -> m RasterJob
runRasterJobMonad job code = execStateT code job

-- | Create a new rasterJob with default allocation sizes.
newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do  initShapePile     <- newPile :: IO (Pile Shape)
        initTilePile     <- newPile :: IO (Pile TileInfo)
        initGroupPile    <- newPile :: IO (Pile ShapeHeader)
        return RasterJob
            { _rJShapePile       = initShapePile
            , _rJGroupPile       = initGroupPile
            , _rJTilePile        = initTilePile
            , _rJBackgroundColor = clear black
            , _rJShapeMap         = M.empty
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJShapePile
        freePile $ job ^. rJGroupPile
        freePile $ job ^. rJTilePile

-- | Reset pile cursors for the entire job and erase the shape map
resetRasterJob :: MonadIO m => RasterJobMonad s m ()
resetRasterJob =
    do  rJShapePile    %= resetPile
        rJTilePile     %= resetPile
        rJShapeMap     .= M.empty

referenceShape :: Shaper PrimEntry -> Shaper GeoReference
referenceShape (Shaper info primEntry) = Shaper info (primEntry ^. primGeoRef)

appendShapes :: MonadIO m
            => [Shaper GeoReference]
            -> RasterJobMonad DisplaySpace m (Slice (Shaper GeoReference))
appendShapes shapes =
       -- add the shape to the pile of shapes and return a reference to it.
       addListToPileState rJShapePile shapes


tileToRasterJob :: MonadIO m
                => Tile
                -> RasterJobMonad DisplaySpace m ()
tileToRasterJob tile =
  do  let primEntries = tilePrims tile
      slice <- appendShapes $ map referenceShape primEntries
      let tileInfo = TileInfo (tileBox tile) slice 0 0
      addToPileState rJTilePile tileInfo
      return ()

buildRasterJob :: MonadIO m
                => RasterJobInput
                -> [Tile]
                -> m RasterJob
buildRasterJob input tiles =
  do  job <- liftIO newRasterJob
      job' <- runRasterJobMonad job $
                  do groupPile <- liftIO $ listToPile $ input ^. rjiShapes
                     rJBackgroundColor .= input ^. rjiBackgroundColor
                     rJGroupPile .= groupPile
                     mapM tileToRasterJob tiles
      --liftIO $ outputRasterJob job'
      return job'

putStrList :: (Show a) => [a] -> IO ()
putStrList ls =
  do
    putStrLn "["
    forM_ ls $ \ x ->
      putStrLn $ "   " ++ show x
    putStrLn "]"

outputRasterJob :: RasterJob -> IO ()
outputRasterJob job =
  do
    putStrLn "---------------- rJShapePile  ---------------------- "
    print . view rJShapePile $ job
    putStrList =<< (pileToList . view rJShapePile $ job)
    putStrLn "---------------- rJGroupPile ------------------- "
    putStrList =<< (pileToList . view rJGroupPile $ job)
    putStrLn "---------------- rJTilePile ----------------------- "
    putStrList =<< (pileToList . view rJTilePile $ job)

instance StorableM TileInfo where
  sizeOfM _ = do sizeOfM (undefined :: Box IntSpace)
                 sizeOfM (undefined :: Slice ShapeId)
                 sizeOfM (undefined :: CInt)
                 sizeOfM (undefined :: CInt)
  alignmentM _ = do alignmentM (undefined :: Box IntSpace)
                    alignmentM (undefined :: Slice ShapeId)
                    alignmentM (undefined :: CInt)
                    alignmentM (undefined :: CInt)
  peekM = do box    <- peekM
             slice  <- peekM
             hDepth <- peekM
             vDepth <- peekM
             return (TileInfo box slice hDepth vDepth)
  pokeM (TileInfo box slice hDepth vDepth) = do pokeM box
                                                pokeM slice
                                                pokeM hDepth
                                                pokeM vDepth

instance Storable TileInfo where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData TileInfo where
  rnf (TileInfo a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

instance NFData RasterJob where
  rnf (RasterJob a b c d e) = {-a `deepseq`-} b `deepseq` c `deepseq` d `deepseq` e `deepseq` ()
