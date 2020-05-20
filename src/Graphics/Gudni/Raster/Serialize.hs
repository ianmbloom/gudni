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
  , serPictureMems
  , serFacetPile
  , serItemTagPile
  , serSubstanceTagPile
  , serSolidColorPile
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
      -- | A Pile of pictureMemoryReferences
    , _serPictureMems      :: Pile (PictureMemoryReference)
      -- | A list of texture facets collected from the scene.
    , _serFacetPile        :: Pile (HardFacet_ s TextureSpace)
     -- | A pile of every item tag collected from the scene.
    , _serItemTagPile      :: Pile ItemTag
      -- | A pile of every substance collected from the scene.
    , _serSubstanceTagPile :: Pile SubstanceTag
    -- | A list of mask colors referenced by substanceTags
    , _serSolidColorPile   :: Pile Color
    }
makeLenses ''SerialState

instance (NFData token, NFData s) => NFData (SerialState token s) where
  rnf (SerialState a b c d e f g h i) =
      a `deepseq` b {-`deepseq` c-} `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` h `deepseq` i `deepseq` ()

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
              pictMemPile      <- liftIO $ newPile
              facetPile        <- liftIO $ newPile
              itemTagPile      <- liftIO $ newPile
              substanceTagPile <- liftIO $ newPile
              colorPile        <- liftIO $ newPile
              tileTree <- liftIO $ buildTileTreeM canvasSize (rasterizer ^. rasterDeviceSpec . specMaxTileSize) newPile
              state            <- execStateT (buildOverScene rasterizer (fromIntegral <$> canvasSize) sceneWithPictMem) $
                  SerialState
                      { _serTokenMap         = M.empty
                      , _serBackgroundColor  = clearBlack
                      , _serGeometryPile     = geometryPile
                      , _serTileTree         = tileTree
                      , _serPictureMems      = pictMemPile
                      , _serFacetPile        = facetPile
                      , _serItemTagPile      = itemTagPile
                      , _serSubstanceTagPile = substanceTagPile
                      , _serSolidColorPile   = colorPile
                      }
              result <- code pictDataPile state
              liftIO $
                  do freePile $ state ^. serGeometryPile
                     freePile $ state ^. serPictureMems
                     freePile $ state ^. serFacetPile
                     freePile $ state ^. serItemTagPile
                     freePile $ state ^. serSubstanceTagPile
                     freePile $ state ^. serSolidColorPile
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
        -> SerialMonad token s m ()
onShape rasterizer canvasSize substanceTag combineType transformer shape =
  do let transformedOutlines = V.fromList . map (applyTransformer transformer) $ view shapeOutlines shape
         boundingBox = boxOf transformedOutlines
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
                        enclosure = enclose reorderTable maxStrandSize transformedOutlines
                    -- Get the geometry pile.
                    geometryPile <- use serGeometryPile
                    -- Add the shape to the geometry pile.
                    (strandRefs, geometryPile') <- liftIO $ runStateT (appendEnclosure enclosure) geometryPile
                    -- Put the geometry pile back in the monad.
                    serGeometryPile .= geometryPile'
                    return strandRefs
             let itemTags = map (strandInfoTag combineType substanceTagId) strandRefs
             addItem boundingBox itemTags

addHardFacet :: MonadIO m
             => SubstanceTagId
             -> HardFacet_ SubSpace TextureSpace
             -> SerialMonad token SubSpace m ()
addHardFacet substanceTagId hardFacet =
  do facetId <- FacetId . sliceStart <$> addToPileState serFacetPile hardFacet
     let facetTag = facetInfoTag facetId substanceTagId
         boundingBox = boxOf hardFacet
     addItem boundingBox [facetTag]

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
        substanceReference <-
            case baseSubstance of
                Texture pictMemReference ->
                    do -- Get the current usage id.
                       pictMemId  <- TextureId . sliceStart <$> addToPileState serPictureMems pictMemReference
                       return . TextureInfo $ pictMemId
                Solid color ->
                    do colorId <- ColorId . sliceStart <$> addToPileState serSolidColorPile color
                       return . SolidInfo $ colorId
        let substanceTag = substanceInfoToTag substanceReference
        traverseCompoundTree defaultValue transformer (onShape rasterizer canvasSize substanceTag) subTree
        case mToken of
             Nothing -> return ()
             Just token ->
                 do tokenMap <- use serTokenMap
                    -- Store the token in the map.
                    serTokenMap .= M.insert substanceTag token tokenMap
        case baseSubstance of
            Texture pictMemReference ->
                do
                   let -- Transformation information is transfered to the texture here.
                       facets :: [Facet_ (SpaceOf item) TextureSpace]
                       facets = rectangleToFacets fromTextureSpace . pictureTextureSize $ pictMemReference
                       combined :: Transformer (SpaceOf item)
                       combined = CombineTransform transformer subTransform
                       transformedFacets :: [Facet_ (SpaceOf item) TextureSpace]
                       transformedFacets = fmap (applyTransformer combined) facets
                       tesselatedFacets :: [[Facet_ (SpaceOf item) TextureSpace]]
                       tesselatedFacets = fmap (tesselateFacet tolerance) transformedFacets
                       hardFacets :: [HardFacet_ (SpaceOf item) TextureSpace]
                       hardFacets = fmap (hardenFacet) . join $ tesselatedFacets
                   -- Add the new usage of the picture to the pile.
                   Slice substanceTagId _ <- addToPileState serSubstanceTagPile substanceTag
                   mapM (addHardFacet $ SubstanceTagId substanceTagId) hardFacets
                   return ()
            _ ->   return ()

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
       traverseShapeTree (onSubstance rasterizer canvasSize textureSpaceToSubspace (SubSpace tAXICABfLATNESS)) (scene ^. sceneShapeTree)
       liftIO $ putStrLn "===================== Serialize scene end   ====================="

outputSerialState :: (Show s, Show token, Storable (HardFacet_ s TextureSpace))
                     => SerialState token s -> IO ()
outputSerialState state =
  do  putStrLn $ "serTokenMap         " ++ (show . view serTokenMap            $ state)
      putStrLn $ "serBackgroundColor  " ++ (show . view serBackgroundColor     $ state)
      putStrLn "---------------- serPictureMems -----------------------"
      putStrList =<< (pileToList . view serPictureMems      $ state)
      putStrLn "---------------- serFacetPile -----------------------"
      putStrList =<< (pileToList . view serFacetPile        $ state)
      putStrLn "---------------- serSubstanceTagPile -----------------------"
      putStrList =<< (pileToList . view serSubstanceTagPile $ state)
      putStrLn "---------------- serSolidColorPile -----------------------"
      putStrList =<< (pileToList . view serSolidColorPile   $ state)
      --putStrLn "---------------- serTileTree -----------------------"
      --putStrLn . show . view serTileTree       $ state
      --putStrLn "---------------- geoGeometryPile -----------------------"
      --putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
