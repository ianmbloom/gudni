{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Serialize
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Serializing a scene into data buffers that can be parsed by the rasterizer kernel and building
-- a partitioned tree of tiles.

module Graphics.Gudni.Raster.Serialize
  ( GeometryPile(..)
  , GeometryState(..)
  , geoReorderTable
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
  , buildOverScene
  , SubstanceState(..)
  , suSubstanceId
  , suTokenMap
  , suCurrentPictureUsage
  , suPictureUsages
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
import Graphics.Gudni.Raster.ReorderTable
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

-- | GeometryPile is just a synonym for a bytepile that stores serialized geometry data for the scene.
type GeometryPile = BytePile
-- add the enclosure data to the geometry pile
appendGeoRef :: Enclosure
             -> StateT GeometryPile IO GeoReference
appendGeoRef enclosure =
    do  geometryPile <- get
        (pile', offsetShapeStartBytes) <- liftIO $ addToBytePile "appendGeoRef" geometryPile enclosure
        put pile'
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 SubSpace) * 2)
        return $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)

-- | Append a shape to the Geometry pile and return a ShapeEntry that contains a reference to its geometry data, the strand count
-- and a the bounding box. This can be added to the tiletree to determine in which tiles the shape is present.
makeShapeEntry :: BoundingBox
               -> Enclosure
               -> StateT GeometryPile IO ShapeEntry
makeShapeEntry box enclosure =
    do  -- append the geometric enclosure data to the heap and return a reference
        geoRef <- appendGeoRef enclosure
        return $ ShapeEntry geoRef (enclosureNumStrands enclosure) box

-- | Return True if a BoundingBox is outside of the canvas.
excludeBox :: Point2 SubSpace
           -> BoundingBox
           -> Bool
excludeBox canvasSize box =
           box ^. leftSide   >= canvasSize ^. pX
        || box ^. topSide    >= canvasSize ^. pY
        || box ^. rightSide  <= 0
        || box ^. bottomSide <= 0

-- | A constructor for holding the state of serializing the geometry from a scene.
data GeometryState = GeometryState
    { _geoReorderTable    :: ReorderTable
    , _geoMaxStrandSize :: Int
    , _geoGeometryPile  :: GeometryPile
    , _geoTileTree      :: TileTree
    , _geoCanvasSize    :: Point2 SubSpace
    , _geoRandomField   :: RandomField
    }
makeLenses ''GeometryState

-- | A monad for passing a GeometryState
type GeometryMonad m = StateT GeometryState m

-- | Function for initializing the geometry monad and running a function inside of it∘
-- The geoTileTree and geoCanvas size must be defined later.
runGeometryMonad :: (MonadIO m)
                 => RandomField
                 -> StateT GeometryState m t
                 -> m t
runGeometryMonad randomField mf =
  do geometryPile <- liftIO (newPileSize iNITgEOMETRYpILEsIZE :: IO BytePile)
     let reorderTable = buildReorderTable mAXsECTIONsIZE
     let geometryState = GeometryState reorderTable mAXsECTIONsIZE geometryPile undefined undefined randomField
     evalStateT mf geometryState

-- | Reuse the geometry monad without reallocating the geometry pile.
resetGeometryMonad :: (MonadIO m)
                   => StateT GeometryState m ()
resetGeometryMonad = do geoGeometryPile %= resetPile

-- | Build a shape with the supplied metadata and representation type.
makeShape :: SubstanceId
          -> Substance a
          -> Compound
          -> rep
          -> Shape rep
makeShape substanceId substance combineType rep = Shape (ShapeInfo (substanceToSubstanceType substance) combineType substanceId) rep

-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
onShape :: MonadIO m
        => (Compound -> ShapeEntry -> Shape ShapeEntry)
        -> Compound
        -> Transformer SubSpace
        -> RawShape
        -> GeometryMonad m ()
onShape wrapShape combineType transformer rawShape =
  do let outlines = Group $ rawShapeToOutlines rawShape
         transformedOutlines = fmap (applyTransformer transformer) outlines
         boundingBox = getBoundingBox transformedOutlines
     canvasSize <- use geoCanvasSize
     if excludeBox canvasSize boundingBox
     then return ()
     else do -- Table used to convert strands of coordinates to trees.
             reorderTable <- use geoReorderTable
             -- Maximum size of a strand.
             maxStrandSize <- use geoMaxStrandSize
             -- Build an enclosure from the outlines.
             let enclosure = enclose reorderTable maxStrandSize (unGroup transformedOutlines)
             -- Get the geometry pile.
             geometryPile <- use geoGeometryPile
             -- Add the shape to the geometry pile.
             (entry, geometryPile') <- liftIO $ runStateT (makeShapeEntry boundingBox enclosure) $ geometryPile
             -- Put the geometry pile back in the monad.
             geoGeometryPile .= geometryPile'
             -- Get the tiletree.
             tileTree <- use geoTileTree
             -- Add the shape to the tile tree.
             geoTileTree .= addShapeToTree tileTree (wrapShape combineType entry)

-- | Constructor for holding the state of serializing substance information from the scene.
data SubstanceState token = SubstanceState
    { -- | The current substance id incremented  with each new substance.
      _suSubstanceId       :: SubstanceId
      -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
    , _suTokenMap          :: M.Map token SubstanceId
      -- | A the latest id for a usage of a picture source, incremented with each new usage of a picture by a new substance from the scene.
    , _suCurrentPictureUsage :: Int
      -- | A list of picture references collected from the scene.
    , _suPictureUsages     :: Pile (PictureUsage PictureMemoryReference)
      -- | The picture memory objects for the scene.
    , _suPictureMems       :: [PictureMemoryReference]
      -- | The background color for the scene.
    , _suBackgroundColor   :: Color
      -- | A pile of every substance collected from the scene.
    , _suSubstancePile     :: Pile SubstanceInfo
    }
makeLenses ''SubstanceState

instance NFData token => NFData (SubstanceState token) where
  rnf (SubstanceState a b c d e f g) =
      a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` ()

-- | A monad for serializing substance data from a scene.
type SubstanceMonad token m = StateT (SubstanceState token) m

-- | Function for executing a new SubstanceMonad
execSubstanceMonad :: MonadIO m
                  => [PictureMemoryReference]
                  -> SubstanceMonad token m a
                  -> m (SubstanceState token)
execSubstanceMonad pictureMems mf =
  do substancePile <- liftIO $ newPile
     pictUsagePile <- liftIO $ newPile
     execStateT mf (SubstanceState (SubstanceId 0) M.empty 0 pictUsagePile pictureMems clearBlack substancePile)

-- | For each shape in the shapeTree add the serialize the substance metadata and serialize the compound subtree.
onSubstance :: (MonadIO m, Ord token)
            => ((Compound -> ShapeEntry -> Shape ShapeEntry)
            -> Compound -> Transformer SubSpace -> item -> GeometryMonad m ())
            -> ()
            -> Transformer SubSpace
            -> SRep token (PictureUsage PictId) (STree Compound (Transformer SubSpace) item)
            -> SubstanceMonad token (GeometryMonad m) ()
onSubstance onShape () transformer (SRep token substance subTree) =
    do  -- Get the current substanceId
        substanceId  <- use suSubstanceId
        -- Increment it for the next shape.
        suSubstanceId += 1
        -- Get the token map.
        tokenMap <- use suTokenMap
        -- Store the token in the map.
        suTokenMap .= M.insert token substanceId tokenMap
        -- Depending on the substance of the shape take appropriate actions.
        colorOrPicture <-
           case substance of
               Texture pictureRef -> do
                   -- get the picture memories.
                   mems <- use suPictureMems
                   -- This would be a good point to put the transformation information from the shape into the texture reference.
                   let pictId = pictSource pictureRef
                       newUsage = pictureRef { pictSource = mems !! fromIntegral pictId
                                             , pictTranslate = zeroPoint}
                   -- Add the new usage of the picture to the pile.
                   addToPileState suPictureUsages newUsage
                   -- Get the current usage id.
                   current <- use suCurrentPictureUsage
                   -- Increment for the next usage.
                   suCurrentPictureUsage += 1
                   -- return a Substance with the right usage id.
                   return . Texture . fromIntegral $ current
               Solid color -> return $ Solid color
        -- Add the new substance to the pile.
        addToPileState suSubstancePile (SubstanceInfo colorOrPicture)
        -- Make a closure to pass to the onShape monad with the metadata for the shape.
        let wrapShape = makeShape substanceId substance
        -- Traverse the compound tree and serialize each component shape.
        lift $ traverseCompoundTree defaultValue transformer (onShape wrapShape) subTree

buildOverScene :: (MonadIO m, Ord token)
                   => Scene token
                   -> SubstanceMonad token (GeometryMonad m) ()
buildOverScene scene =
  do  -- Move the backgound color into the serializer state.
      suBackgroundColor .= scene ^. sceneBackgroundColor
      -- Serialize the shape tree.
      traverseShapeTree (onSubstance onShape) $ scene ^. sceneShapeTree


outputGeometryState :: GeometryState -> IO ()
outputGeometryState state =
  do  putStrLn "---------------- geoGeometryPile -----------------------"
      putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
      --putStrLn "---------------- ReorderTable --------------------------"
      --putStrLn . show . view geoReorderTable     $ state
      --putStrLn . show . view geoMaxStrandSize $ state
      --putStrLn . show . view geoTileTree       $ state
      --putStrLn . show . view geoCanvasSize     $ state
      --putStrLn . show . view geoRandomField    $ state

outputSubstanceState :: Show token => SubstanceState token -> IO ()
outputSubstanceState state =
  do  putStrLn $ "suSubstanceId      " ++ (show . view suSubstanceId         $ state)
      putStrLn $ "suTokenMap         " ++ (show . view suTokenMap            $ state)
      putStrLn $ "suCurrentPictureRef" ++ (show . view suCurrentPictureUsage $ state)
      putStrLn $ "suPictureMems      " ++ (show . view suPictureMems         $ state)
      putStrLn $ "suBackgroundColor  " ++ (show . view suBackgroundColor     $ state)
      putStrLn "---------------- suPictureUsages -----------------------"
      putStrList =<< (pileToList . view suPictureUsages $ state)
      putStrLn "---------------- suSubstancePile -----------------------"
      putStrList =<< (pileToList . view suSubstancePile $ state)
