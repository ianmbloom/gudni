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
  , suTokenMap
  , suBackgroundColor
  , suPictureMems
  , suFacetPile
  , suItemTagPile
  , suSubstanceTagPile
  , suSolidColorPile

  , outputGeometryState
  , outputSubstanceState
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

import Control.Monad
import Control.Monad.State
import Control.Lens
import Foreign.Storable
import Control.DeepSeq

import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified Data.Sequence as S
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
               (geometryPile', ref) <- liftIO $ addToBytePile "appendEnclosure" geometryPile strand
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

-- | A constructor for holding the state of serializing the geometry from a scene.
data GeometryState = GeometryState
    { _geoReorderTable      :: ReorderTable
    , _geoMaxStrandSize     :: Int
    , _geoGeometryPile      :: GeometryPile
    , _geoTileTree          :: TileTree (Tile, S.Seq ItemTagId)
    , _geoCanvasSize        :: Point2 SubSpace
    , _geoRandomField       :: RandomField
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

-- | Constructor for holding the state of serializing substance information from the scene.
data SubstanceState token s = SubstanceState
    { -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
      _suTokenMap          :: M.Map SubstanceTag token
      -- | A Pile of pictureMemoryReferences
    , _suPictureMems       :: Pile (PictureMemoryReference)
      -- | A list of texture facets collected from the scene.
    , _suFacetPile         :: Pile (HardFacet_ s TextureSpace)
      -- | The background color for the scene.
    , _suBackgroundColor   :: Color
     -- | A pile of every item tag collected from the scene.
    , _suItemTagPile :: Pile ItemTag
      -- | A pile of every substance collected from the scene.
    , _suSubstanceTagPile     :: Pile SubstanceTag
    -- | A list of mask colors referenced by substanceTags
    , _suSolidColorPile       :: Pile Color
    }
makeLenses ''SubstanceState

instance (NFData token, NFData s) => NFData (SubstanceState token s) where
  rnf (SubstanceState a b c d e f g) =
      a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` g `deepseq` ()

-- | A monad for serializing substance data from a scene.
type SubstanceMonad token s m = StateT (SubstanceState token s) m

-- | Function for executing a new SubstanceMonad
execSubstanceMonad ::( MonadIO m
                     , Storable (HardFacet_ s TextureSpace)
                     )
                   => SubstanceMonad token s m a
                   -> m (SubstanceState token s)
execSubstanceMonad mf =
  do itemTagPile <- liftIO $ newPile
     substanceTagPile <- liftIO $ newPile
     pictMemPile <- liftIO $ newPile
     facetPile <- liftIO $ newPile
     colorPile <- liftIO $ newPile
     execStateT mf (SubstanceState M.empty pictMemPile facetPile clearBlack itemTagPile substanceTagPile colorPile)

addItem :: MonadIO m
        => BoundingBox
        -> [ItemTag]
        -> SubstanceMonad token s (GeometryMonad m) ()
addItem boundingBox itemTags =
  forM_ itemTags $ \itemTag ->
     do itemTagId <- ItemTagId <$> addToPileState suItemTagPile itemTag
        lift $ do tileTree <- use geoTileTree
                  geoTileTree .= addItemToTree tileTree boundingBox itemTagId -- the maximum strands a facet can create is 3

-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
onShape :: MonadIO m
        => SubstanceTag
        -> Compound
        -> Transformer SubSpace
        -> Shape SubSpace
        -> SubstanceMonad token s (GeometryMonad m) ()
onShape substanceTag combineType transformer shape =
  do substanceTagId <- SubstanceTagId <$> addToPileState suSubstanceTagPile substanceTag
     let transformedOutlines = V.fromList . map (applyTransformer transformer) $ view shapeOutlines shape
         boundingBox = boxOf transformedOutlines
     canvasSize <- lift $ use geoCanvasSize
     if excludeBox canvasSize boundingBox
     then return ()
     else do strandRefs <-
                 lift $ do -- Table used to convert strands of coordinates to trees.
                           reorderTable <- use geoReorderTable
                           -- Maximum size of a strand.
                           maxStrandSize <- use geoMaxStrandSize
                           -- Build an enclosure from the outlines.
                           let enclosure = enclose reorderTable maxStrandSize transformedOutlines
                           -- Get the geometry pile.
                           geometryPile <- use geoGeometryPile
                           -- Add the shape to the geometry pile.
                           (strandRefs, geometryPile') <- liftIO $ runStateT (appendEnclosure enclosure) geometryPile
                           -- Put the geometry pile back in the monad.
                           geoGeometryPile .= geometryPile'
                           return strandRefs
             let itemTags = map (strandInfoTag combineType substanceTagId) strandRefs
             addItem boundingBox itemTags

addHardFacet :: MonadIO m
             => SubstanceTagId
             -> HardFacet_ SubSpace TextureSpace
             -> SubstanceMonad token SubSpace (GeometryMonad m) ()
addHardFacet substanceTagId hardFacet =
  do facetId <- FacetId <$> addToPileState suFacetPile hardFacet
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
            => (TextureSpace -> SpaceOf item)
            -> (SpaceOf item)
            -> Overlap
            -> Transformer (SpaceOf item)
            -> SRep token PictureMemoryReference (STree Compound item)
            -> SubstanceMonad token (SpaceOf item) (GeometryMonad m) ()
onSubstance fromTextureSpace tolerance Overlap transformer (SRep mToken substance subTree) =
    do  -- Depending on the substance of the shape take appropriate actions.
        let (subTransform, baseSubstance) = breakdownSubstance substance
        substanceReference <-
            case baseSubstance of
                Texture pictMemReference ->
                    do -- Get the current usage id.
                       pictMemId <- TextureId <$> addToPileState suPictureMems pictMemReference
                       return $ TextureInfo pictMemId
                Solid color ->
                    do colorId <- ColorId <$> addToPileState suSolidColorPile color
                       return $ SolidInfo colorId
        let substanceTag = substanceInfoToTag substanceReference
        traverseCompoundTree defaultValue transformer (onShape substanceTag) subTree
        case mToken of
             Nothing -> return ()
             Just token ->
                 do tokenMap <- use suTokenMap
                    -- Store the token in the map.
                    suTokenMap .= M.insert substanceTag token tokenMap
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
                   substanceTagId <- SubstanceTagId <$> addToPileState suSubstanceTagPile substanceTag
                   mapM (addHardFacet substanceTagId) hardFacets
                   return ()
            _ ->   return ()

buildOverScene :: (MonadIO m, Show token)
               => Scene (ShapeTreePictureMemory token SubSpace)
               -> SubstanceMonad token SubSpace (GeometryMonad m) ()
buildOverScene scene =
  do   -- Move the backgound color into the serializer state.
       suBackgroundColor .= scene ^. sceneBackgroundColor
       -- Serialize the shape tree.
       traverseShapeTree (onSubstance textureSpaceToSubspace (SubSpace tAXICABfLATNESS)) (scene ^. sceneShapeTree)

outputGeometryState :: GeometryState -> IO ()
outputGeometryState state =
  do  --putStrLn "---------------- geoGeometryPile -----------------------"
      --putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
      --putStrLn "---------------- geoReorderTable --------------------------"
      --putStrLn . show . view geoReorderTable     $ state
      --putStrLn "---------------- geoMaxStrandSize -----------------------"
      --putStrLn . show . view geoMaxStrandSize $ state
      putStrLn "---------------- geoTileTree -----------------------"
      putStrLn . show . view geoTileTree       $ state
      --putStrLn "---------------- geoCanvasSize -----------------------"
      --putStrLn . show . view geoCanvasSize     $ state
      --putStrLn . show . view geoRandomField    $ state

outputSubstanceState :: (Show s, Show token, Storable (HardFacet_ s TextureSpace))
                     => SubstanceState token s -> IO ()
outputSubstanceState state =
  do  putStrLn $ "suTokenMap         " ++ (show . view suTokenMap            $ state)
      putStrLn $ "suBackgroundColor  " ++ (show . view suBackgroundColor     $ state)
      putStrLn "---------------- suPictureMems -----------------------"
      putStrList =<< (pileToList . view suPictureMems      $ state)
      putStrLn "---------------- suFacetPile -----------------------"
      putStrList =<< (pileToList . view suFacetPile        $ state)
      putStrLn "---------------- suSubstanceTagPile -----------------------"
      putStrList =<< (pileToList . view suSubstanceTagPile $ state)
      putStrLn "---------------- suSolidColorPile -----------------------"
      putStrList =<< (pileToList . view suSolidColorPile   $ state)
