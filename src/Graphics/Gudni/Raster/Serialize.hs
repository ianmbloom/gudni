{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
-- Functions used by TraverseShapeTree to serialize a scene into data buffers that can be parsed by
-- the rasterizer kernel and building a partitioned tree of tiles.

module Graphics.Gudni.Raster.Serialize
  ( GeometryPile(..)
  , SerialMonad(..)
  , withSerializedScene
  , buildOverScene
  , SerialState(..)
  , serTokenMap
  , serBackgroundColor
  , serFacetPile
  , serItemTagPile
  , serSubstanceTagPile
  , serDescriptionPile
  , serGeometryPile
  , serTileTree
  , outputSerialState
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Facet

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.OpenCL.Rasterizer

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens
import Foreign.Storable
import Control.DeepSeq

import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified Data.Sequence as S
import Data.Word
import Data.Maybe
import Data.List

import Control.Parallel.Strategies

-- | GeometryPile is just a synonym for a bytepile that stores serialized geometry data for the scene.
type GeometryPile = BytePile
-- add the enclosure data to the geometry pile
appendEnclosure :: Enclosure
                -> StateT GeometryPile IO [StrandReference]
appendEnclosure enclosure =
    do  strandRefs <- forM (enclosureStrands enclosure) ( \strand ->
           do  geometryPile <- get
               (geometryPile', Slice ref breadth) <- liftIO $ addToPile geometryPile (asBytes strand)
               put geometryPile'
               return ref
           )
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let sizePair = fromIntegral (sizeOf (undefined :: Point2 SubSpace) * 2)
            adjustedRefs = fmap (StrandRef . fromIntegral . (`div` sizePair)) strandRefs
        return adjustedRefs

-- | Return True if a BoundingBox is outside of the canvas.
excludeBox :: Point2 SubSpace
           -> BoundingBox
           -> Bool
excludeBox canvasSize box =
           box ^. leftSide   >= canvasSize ^. pX
        || box ^. topSide    >= canvasSize ^. pY
        || box ^. rightSide  <= 0
        || box ^. bottomSide <= 0

-- | Constructor for holding the state of serializing substance information from the scene.
data SerialState token s = SerialState
    { -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
      _serTokenMap         :: M.Map SubstanceTag token
      -- | The background color for the scene.
    , _serBackgroundColor  :: Color
      -- | The tree of tiles collecting itemTagIds
    , _serTileTree         :: TileTree (Tile, Pile ItemTagId)
      -- | The pile of geometry strands
    , _serGeometryPile     :: GeometryPile
      -- | A list of texture facets collected from the scene.
    , _serFacetPile        :: Pile (HardFacet_ s)
     -- | A pile of every item tag collected from the scene.
    , _serItemTagPile      :: Pile ItemTag
      -- | A pile of every substance collected from the scene.
    , _serSubstanceTagPile :: Pile SubstanceTag
      -- | A heap of all substance descriptions
    , _serDescriptionPile  :: BytePile
    }
makeLenses ''SerialState

instance (NFData token, NFData s) => NFData (SerialState token s) where
  rnf (SerialState a b c d e f g h) =
      a `deepseq` b {-`deepseq` c -} `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` h `deepseq` ()

-- | A monad for serializing substance data from a scene.
type SerialMonad token s m = StateT (SerialState token s) m

-- | Function for executing a new SerialMonad
withSerializedScene :: ( MonadIO m
                       , Show token
                       )
                    => Rasterizer
                    -> Point2 PixelSpace
                    -> PictureMap
                    -> Scene (ShapeTree token SubSpace)
                    -> (Pile Word8 -> SerialState token SubSpace -> m a)
                    -> m a
withSerializedScene rasterizer canvasSize pictureMap scene code =
    withScenePictureMemory pictureMap scene $
       \ sceneWithPictMem pictDataPile ->
           do geometryPile     <- liftIO (newPileSize iNITgEOMETRYpILEsIZE :: IO BytePile)
              facetPile        <- liftIO $ newPile
              itemTagPile      <- liftIO $ newPile
              substanceTagPile <- liftIO $ newPile
              descriptionPile <- liftIO $ newPile
              tileTree <- liftIO $ buildTileTreeM canvasSize (rasterizer ^. rasterDeviceSpec . specMaxTileSize) newPile
              state            <- execStateT (buildOverScene rasterizer (fromIntegral <$> canvasSize) sceneWithPictMem) $
                  SerialState
                      { _serTokenMap         = M.empty
                      , _serBackgroundColor  = clearBlack
                      , _serGeometryPile     = geometryPile
                      , _serTileTree         = tileTree
                      , _serFacetPile        = facetPile
                      , _serItemTagPile      = itemTagPile
                      , _serSubstanceTagPile = substanceTagPile
                      , _serDescriptionPile  = descriptionPile
                      }
              result <- code pictDataPile state
              liftIO $
                  do freePile $ state ^. serGeometryPile
                     freePile $ state ^. serFacetPile
                     freePile $ state ^. serItemTagPile
                     freePile $ state ^. serSubstanceTagPile
                     freePile $ state ^. serDescriptionPile
                     --traverseTileTree (\(tile, pile) -> freePile pile) tileTree
              return result

addItem :: MonadIO m
        => BoundingBox
        -> [ItemTag]
        -> SerialMonad token s m ()
addItem boundingBox itemTags =
  forM_ itemTags $ \itemTag ->
     do itemTagId <- ItemTagId . sliceStart <$> addToPileState serItemTagPile itemTag
        tileTree <- use serTileTree
        tileTree' <- addItemTagIdToTreePile tileTree boundingBox itemTagId -- the maximum strands a facet can create is 3
        serTileTree .= tileTree'

-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
onShape :: MonadIO m
        => Rasterizer
        -> Point2 SubSpace
        -> SubstanceTag
        -> Compound
        -> Transformer SubSpace
        -> Shape SubSpace
        -> SerialMonad token s m (Maybe BoundingBox)
onShape rasterizer canvasSize substanceTag combineType transformer shape =
  do let transformedOutlines = map (mapOverPoints (fmap clampReasonable) . applyTransformer transformer) $ view shapeOutlines shape
         boundingBox = minMaxBoxes . fmap boxOf $ transformedOutlines
     if excludeBox canvasSize boundingBox
     then return ()
     else do substanceTagId <- SubstanceTagId . sliceStart <$> addToPileState serSubstanceTagPile substanceTag
             strandRefs <-
                 do -- Build an enclosure from the outlines.
                    let -- Table used to convert strands of coordinates to trees.
                        reorderTable = rasterizer ^. rasterReorderTable
                        -- Maximum size of a strand.
                        maxStrandSize = rasterizer ^. rasterDeviceSpec . specMaxStrandSize
                        -- Turn the shape into a series of strands.
                        enclosure = enclose reorderTable maxStrandSize $ V.fromList transformedOutlines
                    -- Get the geometry pile.
                    geometryPile <- use serGeometryPile
                    -- Add the shape to the geometry pile.
                    (strandRefs, geometryPile') <- liftIO $ runStateT (appendEnclosure enclosure) geometryPile
                    -- Put the geometry pile back in the monad.
                    serGeometryPile .= geometryPile'
                    return strandRefs
             let itemTags = map (strandInfoTag combineType substanceTagId) strandRefs
             addItem boundingBox itemTags
     return $ Just boundingBox

addHardFacet :: MonadIO m
             => SubstanceTagId
             -> HardFacet_ SubSpace
             -> SerialMonad token SubSpace m ()
addHardFacet substanceTagId hardFacet =
  do facetId <- FacetId . sliceStart <$> addToPileState serFacetPile hardFacet
     let facetTag = facetInfoTag facetId substanceTagId
         boundingBox = boxOf hardFacet
     addItem boundingBox [facetTag]


combineBoxes compound = liftA2 minMaxBox

-- | For each shape in the shapeTree serialize the substance metadata and serialize the compound subtree.
onSubstance :: forall m item token .
             ( item ~ Shape SubSpace
             --, SpaceOf item ~ SpaceOf ShapeEntry
             , Space (SpaceOf item)
             , SpaceOf item ~ SubSpace
             , MonadIO m)
            => Rasterizer
            -> Point2 SubSpace
            -> (TextureSpace -> SpaceOf item)
            -> (SpaceOf item)
            -> Overlap
            -> Transformer (SpaceOf item)
            -> SRep token PictureMemoryReference (STree Compound item)
            -> SerialMonad token (SpaceOf item) m ()
onSubstance rasterizer canvasSize fromTextureSpace tolerance Overlap transformer (SRep mToken substance subTree) =
    do  -- Depending on the substance of the shape take appropriate actions.
        let (subTransform, baseSubstance) = breakdownSubstance substance
        descriptionReference <- sliceStart <$> addToPileState serDescriptionPile (asBytes baseSubstance)
        let substanceTag = substanceAndRefToTag baseSubstance descriptionReference
        mShapeBox <- traverseCompoundTree defaultValue transformer (onShape rasterizer canvasSize substanceTag) combineBoxes Nothing subTree
        case mToken of
             Nothing -> return ()
             Just token ->
                 do tokenMap <- use serTokenMap
                    -- Store the token in the map.
                    serTokenMap .= M.insert substanceTag token tokenMap
        -- if the substance is constant (a solid color) we don't need to create facets for it.
        if substanceIsConstant baseSubstance
        then return ()
        else
           do  -- likewise if there is no bounding box for the shape we don't need to create facets for it.
               case mShapeBox of
                    Nothing -> return ()
                    Just shapeBox ->
                         do let -- Transformation information is transfered to the texture here.
                                -- First create two facets that cover the entire bounding box of the shape.
                                facets :: [Facet_ (SpaceOf item)]
                                facets = rectangleToFacets shapeBox
                                -- combine the overall transformation with the substances transformations
                                combined :: Transformer (SpaceOf item)
                                combined = CombineTransform transformer subTransform
                                -- apply all transformations to the facets
                                transformedFacets :: [Facet_ (SpaceOf item)]
                                transformedFacets = fmap (applyTransformer combined) facets
                                -- tesselate the facets
                                tesselatedFacets :: [[Facet_ (SpaceOf item)]]
                                tesselatedFacets = fmap (tesselateFacet tolerance) transformedFacets
                                -- convert the tesselated facets into hard triangles
                                hardFacets :: [HardFacet_ (SpaceOf item)]
                                hardFacets = fmap (hardenFacet) . join $ tesselatedFacets
                            -- Add a new substanceTag to the pile for this group of f
                            Slice substanceTagId _ <- addToPileState serSubstanceTagPile substanceTag
                            -- Add all hard facets with the new substance tag id.
                            mapM (addHardFacet $ SubstanceTagId substanceTagId) hardFacets
                            return ()

buildOverScene :: (MonadIO m, Show token)
               => Rasterizer
               -> Point2 SubSpace
               -> Scene (ShapeTreePictureMemory token SubSpace)
               -> SerialMonad token SubSpace m ()
buildOverScene rasterizer canvasSize scene =
  do   -- Move the backgound color into the serializer state.
       liftIO $ putStrLn "===================== Serialize scene start ====================="
       serBackgroundColor .= scene ^. sceneBackgroundColor
       -- Serialize the shape tree.
       traverseShapeTree (onSubstance rasterizer canvasSize textureSpaceToSubspace (SubSpace $ realToFrac tAXICABfLATNESS)) (\ _ _ _ -> ()) () (scene ^. sceneShapeTree)
       liftIO $ putStrLn "===================== Serialize scene end   ====================="

outputSerialState :: (Show s, Show token, Storable (HardFacet_ s))
                     => SerialState token s -> IO ()
outputSerialState state =
  do  putStrLn $ "serTokenMap         " ++ (show . view serTokenMap            $ state)
      putStrLn $ "serBackgroundColor  " ++ (show . view serBackgroundColor     $ state)
      putStrLn "---------------- serPictureMems -----------------------"
      putStrList =<< (pileToList . view serDescriptionPile      $ state)
      putStrLn "---------------- serFacetPile -----------------------"
      putStrList =<< (pileToList . view serFacetPile        $ state)
      putStrLn "---------------- serSubstanceTagPile -----------------------"
      putStrList =<< (pileToList . view serSubstanceTagPile $ state)
      --putStrLn "---------------- serTileTree -----------------------"
      --putStrLn . show . view serTileTree       $ state
      --putStrLn "---------------- geoGeometryPile -----------------------"
      --putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
