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
  , rjiTileArray
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
  , resizeTileArray
  , freeRasterJob
  , outputRasterJob
  , divideTiles
  , buildRasterJobs
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
import Graphics.Gudni.Raster.TileArray
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
  , _rjiTileArray       :: TileArray
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

jobMessage :: String -> RasterJobMonad s IO ()
jobMessage message = liftIO $ putStrLn message

instance NFData RasterJob where
  rnf (RasterJob a b c d e f g h) = {-a `deepseq`-} b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` h `deepseq` ()

newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do
        initGeometryPile <- newPileSize 65536 :: IO BytePile
        initShapePile     <- newPile :: IO (Pile Shape)
        initShapeRefPile  <- newPile :: IO (Pile (Reference Shape))
        initTilePile     <- newPile :: IO (Pile (Slice ShapeId))
        initGroupPile    <- newPile :: IO (Pile ShapeHeader)
        return RasterJob
            { _rJGeometryPile    = initGeometryPile
            , _rJShapePile        = initShapePile
            , _rJShapeRefPile     = initShapeRefPile
            , _rJGroupPile       = initGroupPile
            , _rJTilePile        = initTilePile
            , _rJTileIndexList   = []
            , _rJBackgroundColor = clear black
            , _rJShapeMap         = M.empty
            }

freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJGeometryPile
        freePile $ job ^. rJShapePile
        freePile $ job ^. rJGroupPile
        freePile $ job ^. rJShapeRefPile
        freePile $ job ^. rJTilePile

resetRasterJob :: RasterJobMonad s IO ()
resetRasterJob =
    do  rJGeometryPile %= resetPile
        rJShapePile     %= resetPile
        rJShapeRefPile  %= resetPile
        rJTilePile     %= resetPile
        rJShapeMap      .= M.empty

-- add the enclosure data to the geometry pile
appendGeoRef :: MonadIO m
             => Int
             -> Enclosure
             -> RasterJobMonad s m (Maybe GeoReference)
appendGeoRef memoryLimit enclosure =
    do  geoPile <- use rJGeometryPile
        if canAddToBytePile geoPile memoryLimit enclosure
        then do (geoPile',  offsetShapeStartBytes) <- liftIO $ addToBytePile "appendGeoDataRef" geoPile enclosure
                rJGeometryPile .= geoPile'
                -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
                let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 DisplaySpace) * 2)
                return $ Just $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)
        else return Nothing

newShape :: MonadIO m
         => Int
         -> PrimId
         -> PrimEnclosure
         -> RasterJobMonad DisplaySpace m (Maybe ShapeId)
newShape memoryLimit primId (primitive, enclosure) =
    do -- append the geometric enclosure data to the heap and return a reference
       mGeoRef <- appendGeoRef memoryLimit enclosure
       case mGeoRef of
         Just geoRef -> do -- add the shape to the pile of shapes and return a reference to it.
                           shapeRef <- addToPileState liftIO rJShapePile (primitive, geoRef)
                           -- add the shapeId to the map between primitive ids and shapeIds
                           rJShapeMap %= M.insert primId shapeRef
                           return $ Just shapeRef
         Nothing -> return Nothing

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

addTileToRasterJob :: MonadIO m => Int
               -> Bag PrimId PrimEnclosure
               -> TileArray
               -> Int
               -> RasterJobMonad DisplaySpace m Bool
addTileToRasterJob memoryLimit primBag tileArray index =
  do  primIds <- lift $ readTile tileArray index
      let primCurves = map (getFromBag primBag) primIds
      mShapeRefs <- mapM (curveToGeoRef memoryLimit) $ zip primIds primCurves
      if any isNothing mShapeRefs
      then return False
      else do slice <- addListToPileState liftIO rJShapeRefPile (catMaybes mShapeRefs)
              addToPileState liftIO rJTilePile slice
              return True

attemptAddTilesToThread :: MonadIO m
                        => Int
                        -> Bag PrimId PrimEnclosure
                        -> TileArray
                        -> [Int]
                        -> RasterJobMonad DisplaySpace m [Int]
attemptAddTilesToThread memoryLimit primBag tileArray (t:ts) =
  do tileAdded <- addTileToRasterJob memoryLimit primBag tileArray t
     if tileAdded
     then do rJTileIndexList %= (fromIntegral t:) -- append t to the tile index list
             attemptAddTilesToThread memoryLimit primBag tileArray ts
     else return (t:ts)
attemptAddTilesToThread memoryLimit primBag tileArray [] =
  return []

buildRasterJobs :: MonadIO m
                => Int
                -> RasterJobInput
                -> [Int]
                -> m [RasterJob]
buildRasterJobs memoryLimit input tileIndices =
  do  job <- liftIO newRasterJob
      (rest, job') <- runRasterJobMonad job $
                           do groupPile <- liftIO $ listToPile $ input ^. rjiShapes
                              rJBackgroundColor .= input ^. rjiBackgroundColor
                              rJGroupPile .= groupPile
                              attemptAddTilesToThread memoryLimit (input ^. rjiPrimBag) (input ^. rjiTileArray) tileIndices
      case rest of
        [] -> return [job']
        rest -> do otherThreads <- buildRasterJobs memoryLimit input rest
                   return $ job' : otherThreads

divideTiles :: MonadIO m
            => CSize
            -> TileArrayMonad m [[Int]]
divideTiles maxGroupSize =
  do  array <- get
      indices <- liftIO $ tileArrayIndices array
      return $ breakList (fromIntegral maxGroupSize) indices

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
