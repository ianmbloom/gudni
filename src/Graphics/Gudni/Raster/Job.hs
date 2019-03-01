{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Raster.Job
  ( GeoReference(..)
  , RasterJobMonad(..)
  , runRasterJobMonad
  , jobMessage
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
  , rJTileIndexList
  , rJBackgroundColor
  , rJShapeMap
  , ShapeId(..)
  , newRasterJob
  , resetRasterJob
  , freeRasterJob
  , outputRasterJob
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Bag
import Graphics.Gudni.Util.Util

import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Primitive

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

data RasterJobInput = RasterJobInput
  { _rjiBackgroundColor :: Color
  , _rjiShapes          :: [ShapeHeader]
  , _rjiPrimBag         :: Bag PrimId PrimEnclosure
  , _rjiTileTree        :: TileTree
  }
makeLenses ''RasterJobInput

data RasterJob = RasterJob
  { _rJGeometryPile    :: !BytePile
  , _rJShapePile       :: !(Pile Shape)
  , _rJShapeRefPile    :: !(Pile ShapeId)
  , _rJTilePile        :: !(Pile (Slice ShapeId))
  , _rJGroupPile       :: !(Pile ShapeHeader)
  , _rJTileIndexList   :: [CUInt]
  , _rJBackgroundColor :: !Color
  , _rJShapeMap        :: M.Map PrimId ShapeId
  } deriving (Show)
makeLenses ''RasterJob

type RasterJobMonad s m = StateT (RasterJob) m

runRasterJobMonad :: MonadIO m => RasterJob -> RasterJobMonad s m a -> m (a, RasterJob)
runRasterJobMonad job code = runStateT code job

instance NFData RasterJob where
  rnf (RasterJob a b c d e f g h) = {-a `deepseq`-} b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` h `deepseq` ()

-- add the enclosure data to the geometry pile
appendGeoRef :: MonadIO m
             => Int
             -> Enclosure
             -> RasterJobMonad s m GeoReference
appendGeoRef memoryLimit enclosure =
    do  geoPile <- use rJGeometryPile
        (geoPile',  offsetShapeStartBytes) <- liftIO $ addToBytePile "appendGeoDataRef" geoPile enclosure
        rJGeometryPile .= geoPile'
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 DisplaySpace) * 2)
        return $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)

newShape :: MonadIO m
         => Int
         -> PrimId
         -> PrimEnclosure
         -> RasterJobMonad DisplaySpace m (Maybe ShapeId)
newShape memoryLimit primId (primitive, enclosure) =
    do -- append the geometric enclosure data to the heap and return a reference
       geoRef <- appendGeoRef memoryLimit enclosure
       -- add the shape to the pile of shapes and return a reference to it.
       shapeRef <- addToPileState liftIO rJShapePile (primitive, geoRef)
       -- add the shapeId to the map between primitive ids and shapeIds
       rJShapeMap %= M.insert primId shapeRef
       return shapeRef

curveToGeoRef :: MonadIO m
              => Int
              -> (PrimId, PrimEnclosure)
              -> RasterJobMonad DisplaySpace m (Maybe ShapeId)
curveToGeoRef memoryLimit (primId, primEnclosure) =
    do  geoMap <- use rJShapeMap
        case M.lookup primId geoMap of
            Just shapeRef -> return $ Just shapeRef
            Nothing       -> newShape memoryLimit primId primEnclosure

offsetShape :: Reference b -> (ShapeHeader, Slice PrimId) -> (ShapeHeader, Slice b)
offsetShape offset (header, Slice ref breadth) = (header, Slice (Ref $ unRef ref+ unRef offset) (Breadth $ unBreadth breadth))

tileToRasterJob :: MonadIO m
                => Int
                -> Bag PrimId PrimEnclosure
                -> Tile
                -> RasterJobMonad DisplaySpace m Bool
tileToRasterJob memoryLimit primBag tile =
  do  let primIds = map primId $ tilePrims tile
          primCurves = map (getFromBag primBag) primIds
      mShapeRefs <- mapM (curveToGeoRef memoryLimit) $ zip primIds primCurves
      if any isNothing mShapeRefs
      then return False
      else do slice <- addListToPileState liftIO rJShapeRefPile (catMaybes mShapeRefs)
              addToPileState liftIO rJTilePile slice
              return True

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
    putStrLn "---------------- rJTileIndexList ----------------------- "
    putStrLn $ show $ view rJTileIndexList job
