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

module Graphics.Gudni.Raster.ConfineTree.Serialize
  ( ConfineMonad(..)
  , withConfinedScene
  , confineOverScene
  , ConfineState(..)
  , conTokenMap
  , conBackgroundColor
  , conColorMap
  , conBezierPile
  , conFacetPile
  , conItemTagPile
  , conSubstanceTagPile
  , conDescriptionPile
  , conCurveTag
  , outputConfineState
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout.FromLayout

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.OpenCL.Rasterizer

import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier

--import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Foreign.Storable
import Control.DeepSeq

import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Sequence as S
import Data.Word
import Data.Maybe
import Data.List


-- | Return True if a BoundingBox is outside of the canvas.
excludeBox :: Maybe (Point2 SubSpace)
           -> BoundingBox
           -> Bool
excludeBox mCanvasSize box =
  case mCanvasSize of
    Nothing -> False
    Just canvasSize ->
           box ^. leftSide   >= canvasSize ^. pX
        || box ^. topSide    >= canvasSize ^. pY
        || box ^. rightSide  <= 0
        || box ^. bottomSide <= 0

-- | Constructor for holding the state of serializing substance information from the scene.
data ConfineState token s = ConfineState
    { -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
      _conTokenMap         :: M.Map SubstanceTag token
      -- | The background color for the scene.
    , _conBackgroundColor  :: Color
      -- | Map from ItemId to color
    , _conColorMap         :: M.Map ItemTagId Color
      -- | A list of every bezier that defines the scene.
    , _conBezierPile       :: Pile (TaggedBezier s)
      -- | A list of texture facets collected from the scene.
    , _conFacetPile        :: Pile (Facet_ s)
     -- | A pile of every item tag collected from the scene.
    , _conItemTagPile      :: Pile ItemTag
      -- | A pile of every substance collected from the scene.
    , _conSubstanceTagPile :: Pile SubstanceTag
      -- | A heap of all substance descriptions
    , _conDescriptionPile  :: BytePile
    , _conCurveTag :: Int
    }
makeLenses ''ConfineState

instance (NFData token, NFData s) => NFData (ConfineState token s) where
  rnf (ConfineState a b c d e f g h i) =
      a `deepseq` b `deepseq` c {-`deepseq` d `deepseq` e-} `deepseq` f `deepseq` g `deepseq` h `deepseq` i `deepseq` ()

-- | A monad for serializing substance data from a scene.
type ConfineMonad token s m = StateT (ConfineState token s) m

-- | Function for executing a new ConfineMonad
withConfinedScene :: ( MonadIO m
                     , Show token
                     )
                  => Maybe (Point2 PixelSpace)
                  -> PictureMap
                  -> Scene (Maybe (FinalTree token SubSpace))
                  -> (Pile Word8 -> ConfineState token SubSpace -> m a)
                  -> m a
withConfinedScene canvasSize pictureMap scene code =
    withScenePictureMemory pictureMap scene $
       \ sceneWithPictMem pictDataPile ->
           do bezierPile       <- liftIO $ newPile
              facetPile        <- liftIO $ newPile
              itemTagPile      <- liftIO $ newPile
              substanceTagPile <- liftIO $ newPile
              descriptionPile  <- liftIO $ newPile
              --tileTree <- liftIO $ buildTileTreeM canvasSize (rasterizer ^. rasterDeviceSpec . specMaxTileSize) newPile
              state            <- execStateT (confineOverScene (fmap fromIntegral <$> canvasSize) sceneWithPictMem) $
                  ConfineState
                      { _conTokenMap         = M.empty
                      , _conBackgroundColor  = clearBlack
                      --, _conTileTree         = tileTree
                      , _conColorMap         = M.empty
                      , _conBezierPile       = bezierPile
                      , _conFacetPile        = facetPile
                      , _conItemTagPile      = itemTagPile
                      , _conSubstanceTagPile = substanceTagPile
                      , _conDescriptionPile  = descriptionPile
                      , _conCurveTag         = 0
                      }
              result <- code pictDataPile state
              liftIO $
                  do freePile $ state ^. conFacetPile
                     freePile $ state ^. conItemTagPile
                     freePile $ state ^. conSubstanceTagPile
                     freePile $ state ^. conDescriptionPile
                     --traverseTileTree (\(tile, pile) -> freePile pile) tileTree
              return result

{-
addItem :: MonadIO m
        => BoundingBox
        -> [ItemTag]
        -> ConfineMonad token s m ()
addItem boundingBox itemTags =
  forM_ itemTags $ \itemTag ->
     do itemTagId <- ItemTagId . sliceStart <$> addToPileState conItemTagPile itemTag
        tileTree <- use conTileTree
        tileTree' <- addItemTagIdToTreePile tileTree boundingBox itemTagId -- the maximum strands a facet can create is 3
        conTileTree .= tileTree'
-}
-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
confineShape :: MonadIO m
             => Maybe (Point2 SubSpace)
             -> Color
             -> SubstanceTag
             -> Compound
             -> Shape SubSpace
             -> ConfineMonad token SubSpace m (Maybe BoundingBox)
confineShape canvasSize color substanceTag combineType shape =
  do let transformedOutlines = view shapeOutlines . mapOverPoints (fmap clampReasonable) $ shape
         boundingBox = minMaxBoxes . fmap boxOf $ transformedOutlines
     if excludeBox canvasSize boundingBox
     then return ()
     else do substanceTagId <- SubstanceTagId . sliceStart <$> addToPileState conSubstanceTagPile substanceTag
             let itemTag = strandInfoTag combineType substanceTagId (StrandRef nullReference)
             itemTagId <- ItemTagId . sliceStart <$> addToPileState conItemTagPile itemTag
             conColorMap %= M.insert itemTagId color
             let curves = V.concat .
                          map (view outlineSegments) $
                          transformedOutlines
                 taggedBeziers = V.map (\curve -> TaggedBezier curve itemTagId) curves
             addFoldableToPileState conBezierPile taggedBeziers
             return ()
     return $ Just boundingBox

{-
addHardFacet :: MonadIO m
             => SubstanceTagId
             -> HardFacet_ SubSpace
             -> ConfineMonad token SubSpace m ()
addHardFacet substanceTagId hardFacet =
  do facetId <- FacetId . sliceStart <$> addToPileState conFacetPile hardFacet
     let facetTag = facetInfoTag facetId substanceTagId
         boundingBox = boxOf hardFacet
     addItem boundingBox [facetTag]
-}

combineBoxes compound = liftA2 minMaxBox

-- | For each shape in the shapeTree serialize the substance metadata and serialize the compound subtree.
confineSubstance :: forall m item token .
                  ( item ~ Shape SubSpace
                  --, SpaceOf item ~ SpaceOf ShapeEntry
                  , Space (SpaceOf item)
                  , SpaceOf item ~ SubSpace
                  , MonadIO m)
                 => Maybe (Point2 SubSpace)
                 -> (TextureSpace -> SpaceOf item)
                 -> (SpaceOf item)
                 -> Overlap
                 -> SRep token PictureMemoryReference (Tree Compound item)
                 -> ConfineMonad token (SpaceOf item) m ()
confineSubstance canvasSize fromTextureSpace tolerance Overlap (SRep mToken substance subTree) =
    do  -- Depending on the substance of the shape take appropriate actions.
        let (subTransform, baseSubstance) = breakdownSubstance substance
        let color = case baseSubstance of
                        Solid c -> c
                        _ -> black
        descriptionReference <- sliceStart <$> addToPileState conDescriptionPile (asBytes baseSubstance)
        let substanceTag = substanceAndRefToTag baseSubstance descriptionReference
        mShapeBox <- traverseTree traverseCompound combineBoxes (confineShape canvasSize color substanceTag) defaultValue subTree
        case mToken of
             Nothing -> return ()
             Just token ->
                 do tokenMap <- use conTokenMap
                    -- Store the token in the map.
                    conTokenMap .= M.insert substanceTag token tokenMap
        -- if the substance is constant (a solid color) we don't need to create facets for it.
        if substanceIsConstant baseSubstance
        then return ()
        else
           do  -- likewise if there is no bounding box for the shape we don't need to create facets for it.
               case mShapeBox of
                    Nothing -> return ()
                    Just shapeBox ->
                         {-
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
                            Slice substanceTagId _ <- addToPileState conSubstanceTagPile substanceTag
                            -- Add all hard facets with the new substance tag id.
                            mapM (addHardFacet $ SubstanceTagId substanceTagId) hardFacets
                            -}
                            return ()

confineOverScene :: forall m token
                 . (MonadIO m, Show token)
                 => Maybe (Point2 SubSpace)
                 -> Scene (FinalTreePictureMemory token SubSpace)
                 -> ConfineMonad token SubSpace m ()
confineOverScene canvasSize scene =
  do   -- Move the backgound color into the serializer state.
       liftIO $ putStrLn "===================== Serialize scene start ====================="
       conBackgroundColor .= scene ^. sceneBackgroundColor
       -- Serialize the shape tree.
       traverseTree keepMeld (const3 ()) (confineSubstance canvasSize textureSpaceToSubspace (SubSpace $ realToFrac tAXICABfLATNESS)) Overlap (scene ^. sceneShapeTree)
       liftIO $ putStrLn $ "===================== Serialize scene end ====================="

outputConfineState :: (Show s, Show token, Storable (Facet_ s))
                   => ConfineState token s -> IO ()
outputConfineState state =
  do  putStrLn $ "conTokenMap         " ++ (show . view conTokenMap            $ state)
      putStrLn $ "conBackgroundColor  " ++ (show . view conBackgroundColor     $ state)
      putStrLn "---------------- conPictureMems -----------------------"
      putStrList =<< (pileToList . view conDescriptionPile      $ state)
      putStrLn "---------------- conFacetPile -----------------------"
      putStrList =<< (pileToList . view conFacetPile        $ state)
      putStrLn "---------------- conSubstanceTagPile -----------------------"
      putStrList =<< (pileToList . view conSubstanceTagPile $ state)
      --putStrLn "---------------- conTileTree -----------------------"
      --putStrLn . show . view conTileTree       $ state
      --putStrLn "---------------- geoGeometryPile -----------------------"
      --putStr =<< fmap unlines (bytePileToGeometry . view geoGeometryPile $ state)
