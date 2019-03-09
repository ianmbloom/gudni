{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Geometry
  ( GeometryPile(..)
  , GeometryState(..)
  , geoCurveTable
  , geoMaxStrandSize
  , geoGeometryPile
  , geoTileTree
  , geoCanvasSize
  , geoRandomField
  , GeometryMonad(..)
  , runGeometryMonad
  , resetGeometryMonad
  , SubstanceMonad(..)
  , execSubstanceMonad
  , buildOverShapeTree
  , SubstanceState(..)
  , suSubstanceId
  , suTokenMap
  , suCurrentPictureRef
  , suPictureRefs
  , suPictureMems
  , suBackgroundColor
  , suSubstancePile
  , outputGeometryState
  , outputSubstanceState
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.ShapeInfo
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad
import Control.Monad.State
import Control.Lens
import Foreign.Storable
import Control.DeepSeq

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Control.Parallel.Strategies

type GeometryPile = BytePile
-- add the enclosure data to the geometry pile
appendGeoRef :: Enclosure
             -> StateT GeometryPile IO GeoReference
appendGeoRef enclosure =
    do  geometryPile <- get
        (pile', offsetShapeStartBytes) <- liftIO $ addToBytePile "appendGeoRef" geometryPile enclosure
        put pile'
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 DisplaySpace) * 2)
        return $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)

makeShapeEntry :: BoundingBox
               -> Enclosure
               -> StateT GeometryPile IO ShapeEntry
makeShapeEntry box enclosure =
    do  -- append the geometric enclosure data to the heap and return a reference
        geoRef <- appendGeoRef enclosure
        return $ ShapeEntry geoRef (enclosureNumStrands enclosure) box

overShape :: (t -> StateT s IO u) -> (Shape t) -> StateT s IO (Shape u)
overShape f (Shape i t) = do u <- f t
                             return $ Shape i u

excludeBox :: Point2 DisplaySpace
           -> BoundingBox
           -> Bool
excludeBox canvasSize box =
           box ^. leftSide   >= canvasSize ^. pX
        || box ^. topSide    >= canvasSize ^. pY
        || box ^. rightSide  <= 0
        || box ^. bottomSide <= 0

data GeometryState = GeometryState
    { _geoCurveTable    :: CurveTable
    , _geoMaxStrandSize :: Int
    , _geoGeometryPile  :: GeometryPile
    , _geoTileTree      :: TileTree
    , _geoCanvasSize    :: Point2 DisplaySpace
    , _geoRandomField   :: RandomField
    }
makeLenses ''GeometryState

type GeometryMonad m = StateT GeometryState m

runGeometryMonad :: (MonadIO m)
                 => RandomField
                 -> StateT GeometryState m t
                 -> m t
runGeometryMonad randomField mf =
  do geometryPile <- liftIO (newPileSize iNITgEOMETRYpILEsIZE :: IO BytePile)
     let curveTable = buildCurveTable mAXsECTIONsIZE
     let geometryState = GeometryState curveTable mAXsECTIONsIZE geometryPile undefined undefined randomField
     evalStateT mf geometryState

resetGeometryMonad :: (MonadIO m)
                   => StateT GeometryState m ()
resetGeometryMonad = do geoGeometryPile %= resetPile

makeShape :: SubstanceId
          -> Substance a
          -> CombineType
          -> rep
          -> Shape rep
makeShape substanceId substance combineType rep = Shape (ShapeInfo (substanceToSubstanceType substance) combineType substanceId) rep

onShape :: MonadIO m
        => (CombineType -> ShapeEntry -> Shape ShapeEntry)
        -> CombineType
        -> TransformType DisplaySpace
        -> RawShape
        -> GeometryMonad m ()
onShape wrapShape combineType transformType rawShape =
  do let outlines = Group $ rawShapeToOutlines rawShape
         transformedOutlines = fmap (applyTransformType transformType) outlines
         boundingBox = getBoundingBox transformedOutlines
     canvasSize <- use geoCanvasSize
     if excludeBox canvasSize boundingBox
     then return ()
     else do -- Table used to convert strands of coordinates to trees.
             curveTable  <- use geoCurveTable
             -- Maximum size of a strand.
             maxStrandSize <- use geoMaxStrandSize
             let enclosure = enclose curveTable maxStrandSize (unGroup transformedOutlines)
             geometryPile <- use geoGeometryPile
             (entry, geometryPile') <- liftIO $ runStateT (makeShapeEntry boundingBox enclosure) geometryPile
             geoGeometryPile .= geometryPile'
             tileTree <- use geoTileTree
             geoTileTree .= addShapeToTree tileTree (wrapShape combineType entry)

data SubstanceState token = SubstanceState
    { _suSubstanceId       :: SubstanceId
    , _suTokenMap          :: M.Map token SubstanceId
    , _suCurrentPictureRef :: Int
    , _suPictureRefs       :: [PictureRef PictureMemory]
    , _suPictureMems       :: [PictureMemory]
    , _suBackgroundColor   :: Color
    , _suSubstancePile     :: Pile SubstanceInfo
    }
makeLenses ''SubstanceState

instance NFData token => NFData (SubstanceState token) where
  rnf (SubstanceState a b c d e f g) =
      a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` ()

type SubstanceMonad token m = StateT (SubstanceState token) m

execSubstanceMonad :: MonadIO m
                  => [PictureMemory]
                  -> SubstanceMonad token m a
                  -> m (SubstanceState token)
execSubstanceMonad pictureMems mf =
  do substancePile <- liftIO $ newPile
     execStateT mf (SubstanceState (SubstanceId 0) M.empty 0 [] pictureMems clearBlack substancePile)

-- | Traverse the ShapeTree while assigning ids to every shapeSubstanceId, recording every tokens relationship to
-- and assigning a picture reference to each referenced picture.

onSubstance :: (MonadIO m, Ord token)
            => ((CombineType -> ShapeEntry -> Shape ShapeEntry)
            -> CombineType -> TransformType DisplaySpace -> item -> GeometryMonad m ())
            -> ()
            -> TransformType DisplaySpace
            -> SRep token (PictureRef PictId) (STree CombineType (TransformType DisplaySpace) item)
            -> SubstanceMonad token (GeometryMonad m) ()
onSubstance onShape () transformType (SRep token substance subTree) =
    do  substanceId  <- use suSubstanceId
        suSubstanceId += 1
        tokenMap <- use suTokenMap
        suTokenMap .= M.insert token substanceId tokenMap
        colorOrPicture <-
           case substance of
               Texture pictureRef -> do
                   mems <- use suPictureMems
                   let pictId = pictData pictureRef
                       newRef = pictureRef { pictData = mems !! fromIntegral pictId
                                           , pictTranslate = zeroPoint}
                   suPictureRefs %= (newRef:)
                   current <- use suCurrentPictureRef
                   suCurrentPictureRef += 1
                   return . Texture . fromIntegral $ current
               Solid color -> return $ Solid color
        addToPileState suSubstancePile (SubstanceInfo colorOrPicture)
        let wrapShape = makeShape substanceId substance
        lift $ traverseCompoundTree defaultValue transformType (onShape wrapShape) subTree

buildOverShapeTree :: (MonadIO m, Ord token)
                   => STreeRoot (STree ()
                                 (TransformType DisplaySpace)
                                 (SRep token (PictureRef PictId)
                                 (STree CombineType (TransformType DisplaySpace) RawShape)))
                   -> SubstanceMonad token (GeometryMonad m) ()
buildOverShapeTree shapeTreeRoot =
  do suBackgroundColor .= shapeTreeRoot ^. rootBackgroundColor
     traverseShapeTree (onSubstance onShape) $ shapeTreeRoot ^. rootShapeTree


outputGeometryState :: GeometryState -> IO ()
outputGeometryState state =
  do  putStrLn "---------------- geoGeometryPile -----------------------"
      putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
      --putStrLn "---------------- curveTable --------------------------"
      --putStrLn . show . view geoCurveTable     $ state
      --putStrLn . show . view geoMaxStrandSize $ state
      putStrLn . show . view geoTileTree       $ state
      --putStrLn . show . view geoCanvasSize     $ state
      --putStrLn . show . view geoRandomField    $ state

outputSubstanceState :: Show token => SubstanceState token -> IO ()
outputSubstanceState state =
  do  putStrLn $ "suSubstanceId      " ++ (show . view suSubstanceId       $ state)
      putStrLn $ "suTokenMap         " ++ (show . view suTokenMap          $ state)
      putStrLn $ "suCurrentPictureRef" ++ (show . view suCurrentPictureRef $ state)
      putStrLn $ "suPictureRefs      " ++ (show . view suPictureRefs       $ state)
      putStrLn $ "suPictureMems      " ++ (show . view suPictureMems       $ state)
      putStrLn $ "suBackgroundColor  " ++ (show . view suBackgroundColor   $ state)
      putStrLn "---------------- suSubstancePile -----------------------"
      putStrList =<< (pileToList . view suSubstancePile $ state)
