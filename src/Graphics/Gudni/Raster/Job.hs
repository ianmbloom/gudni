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
  , rjiPrimBag
  , rjiTileTree
  , RasterJob(..)
  , rJGeometryPile
  , rJShapePile
  , rJShapeRefPile
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
import Graphics.Gudni.Util.Bag
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
  -- | Range of shapeIds in the geometry heap.
  { _tiShapes :: !(Slice ShapeId)
  -- | Pixel boundaries of tile.
  , _tiBox    :: !(Box IntSpace)
  -- | Logarithmic horizontal depth.
  , _tiHDepth :: !(CInt)
  -- | Logarithmic vertical depth.
  , _tiVDepth :: !(CInt)
  } deriving (Show)

data RasterJobInput = RasterJobInput
  { _rjiBackgroundColor :: Color
  , _rjiShapes          :: [ShapeHeader]
  , _rjiPrimBag         :: Bag PrimId (Shaper Enclosure)
  , _rjiTileTree        :: TileTree
  }
makeLenses ''RasterJobInput

data RasterJob = RasterJob
  { _rJGeometryPile    :: !BytePile
  , _rJShapePile       :: !(Pile Shape)
  , _rJShapeRefPile    :: !(Pile ShapeId)
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
    do
        initGeometryPile <- newPileSize 65536 :: IO BytePile
        initShapePile     <- newPile :: IO (Pile Shape)
        initShapeRefPile  <- newPile :: IO (Pile (Reference Shape))
        initTilePile     <- newPile :: IO (Pile TileInfo)
        initGroupPile    <- newPile :: IO (Pile ShapeHeader)
        return RasterJob
            { _rJGeometryPile    = initGeometryPile
            , _rJShapePile       = initShapePile
            , _rJShapeRefPile    = initShapeRefPile
            , _rJGroupPile       = initGroupPile
            , _rJTilePile        = initTilePile
            , _rJBackgroundColor = clear black
            , _rJShapeMap         = M.empty
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJGeometryPile
        freePile $ job ^. rJShapePile
        freePile $ job ^. rJGroupPile
        freePile $ job ^. rJShapeRefPile
        freePile $ job ^. rJTilePile

-- | Reset pile cursors for the entire job and erase the shape map
resetRasterJob :: RasterJobMonad s IO ()
resetRasterJob =
    do  rJGeometryPile %= resetPile
        rJShapePile    %= resetPile
        rJShapeRefPile %= resetPile
        rJTilePile     %= resetPile
        rJShapeMap     .= M.empty


-- add the enclosure data to the geometry pile
appendGeoRef :: MonadIO m
             => Enclosure
             -> RasterJobMonad s m GeoReference
appendGeoRef enclosure =
    do  offsetShapeStartBytes <- addToBytePileState rJGeometryPile enclosure
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 DisplaySpace) * 2)
        return $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)

newShape :: MonadIO m
         => PrimId
         -> Shaper Enclosure
         -> RasterJobMonad DisplaySpace m ShapeId
newShape primId (Shaper shapeInfo enclosure) =
    do -- append the geometric enclosure data to the heap and return a reference
       geoRef <- appendGeoRef enclosure
       -- add the shape to the pile of shapes and return a reference to it.
       shapeRef <- addToPileState rJShapePile (Shaper shapeInfo geoRef)
       -- add the shapeId to the map between primitive ids and shapeIds
       rJShapeMap %= M.insert primId shapeRef
       return shapeRef

curveToGeoRef :: MonadIO m
              => (PrimId, Shaper Enclosure)
              -> RasterJobMonad DisplaySpace m ShapeId
curveToGeoRef (primId, primEnclosure) =
    do  geoMap <- use rJShapeMap
        case M.lookup primId geoMap of
            Just shapeRef -> return $ shapeRef
            Nothing       -> newShape primId primEnclosure

offsetShape :: Reference b -> (ShapeHeader, Slice PrimId) -> (ShapeHeader, Slice b)
offsetShape offset (header, Slice ref breadth) = (header, Slice (Ref $ unRef ref+ unRef offset) (Breadth $ unBreadth breadth))

tileToRasterJob :: MonadIO m
                => Bag PrimId (Shaper Enclosure)
                -> Tile
                -> RasterJobMonad DisplaySpace m ()
tileToRasterJob primBag tile =
  do  let primIds = map (view primId) $ tilePrims tile
          primCurves = map (getFromBag primBag) primIds
      shapeRefs <- mapM curveToGeoRef $ zip primIds primCurves
      slice <- addListToPileState rJShapeRefPile shapeRefs
      let tileInfo = TileInfo slice (tileBox tile) 0 0
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
                     mapM (tileToRasterJob (input ^. rjiPrimBag)) (tr "tiles" tiles)
      liftIO $ outputRasterJob job'
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
    putStrLn "---------------- rJGeometryPile ------------------- "
    print . view rJGeometryPile $ job
    putStr =<< fmap unlines (bytePileToGeometry . view rJGeometryPile $ job)
    putStrLn "---------------- rJShapePile  ---------------------- "
    print . view rJShapePile $ job
    putStrList =<< (pileToList . view rJShapePile $ job)
    putStrLn "---------------- rJShapeRefPile  ------------------- "
    putStrList =<< (pileToList . view rJShapeRefPile $ job)
    putStrLn "---------------- rJGroupPile ------------------- "
    putStrList =<< (pileToList . view rJGroupPile $ job)
    putStrLn "---------------- rJTilePile ----------------------- "
    putStrList =<< (pileToList . view rJTilePile $ job)

instance StorableM TileInfo where
  sizeOfM _ = do sizeOfM (undefined :: Slice ShapeId)
                 sizeOfM (undefined :: Box IntSpace)
                 sizeOfM (undefined :: CInt)
                 sizeOfM (undefined :: CInt)
  alignmentM _ = do alignmentM (undefined :: Slice ShapeId)
                    alignmentM (undefined :: Box IntSpace)
                    alignmentM (undefined :: CInt)
                    alignmentM (undefined :: CInt)
  peekM = do slice  <- peekM
             box    <- peekM
             hDepth <- peekM
             vDepth <- peekM
             return (TileInfo slice box hDepth vDepth)
  pokeM (TileInfo slice box hDepth vDepth) = do pokeM slice
                                                pokeM box
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
  rnf (RasterJob a b c d e f g) = {-a `deepseq`-} b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` ()
