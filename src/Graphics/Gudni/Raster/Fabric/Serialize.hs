{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Serialize
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for serializing a scene into bytecode and confine trees.

module Graphics.Gudni.Raster.Fabric.Serialize
  ( serializeFabric
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Combine.Tag
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Substance.Storage
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Filter.Tag
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Transformer.Storage
import Graphics.Gudni.Raster.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.FromLayout
import Graphics.Gudni.Raster.Storage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Lens
import Foreign.Storable

import qualified Data.Vector as V

data PrimTop s
  = PTag FabricTagId
  | PSlice [Slice PrimTagId]
  deriving (Show)

data SerialState s
   = SerialState
   { _tSPrimTop   :: PrimTop s
   , _tSBox       :: Maybe (Box s)
   }
makeLenses ''SerialState

type SerialMonad s m a = StateT (SerialState s) (DagMonad s m) a

announce :: MonadIO m => String -> m ()
announce mess = liftIO $ putStrLn $ "===================== Serialize Fabric " ++ mess ++ " ====================="

serializeFabric :: forall m style
                .  ( DagConstraints (SpaceOf style) m
                   , IsStyle style
                   )
                => SpaceOf style
                -> Int
                -> Fabric (PicturePass style)
                -> DagMonad (SpaceOf style) m FabricTagId
serializeFabric limit decorateLimit fabric =
    evalStateT (do announce "Start"
                   addReturnTag
                   go fabric
                   makeTree limit decorateLimit
                   announce "End"
                   currentPointer
               ) (SerialState (PTag 0) Nothing)
    where
    go :: Fabric (PicturePass style)
       -> SerialMonad (SpaceOf style) m ()
    go fabric =
        do top <- use tSPrimTop
           -- liftIO $ putStrLn $ "goSerial " ++ showFabricHead fabric
           case fabric of
               FBinary ty above below ->
                   do let melder = ty ^. proxMeld
                      addBinaryTag melder
                      go below
                      go above
               FLeaf leaf ->
                   case leaf of
                       FShape shape ->
                           do  addSubstanceTag (FConst insideShape :: FSubstance (PicturePass style))
                               addShapeToPrims shape
                               addReturnTag
                       FSubstance substance ->
                           do  addSubstanceTag substance
               FUnaryPre trans child ->
                    do jumpTo <- currentPointer
                       addReturnTag
                       go child
                       addReturnTag
                       makeTree limit decorateLimit
                       addTransformTag trans
                       addStackerTag jumpTo
                       setTop
                       tSBox %= fmap (transBoundary trans)
               FUnaryPost filt child ->
                    do addReturnTag
                       go child
                       addReturnTag
                       addFilterTag filt
                       makeTree limit decorateLimit


currentPointer :: (DagConstraints s m) => SerialMonad s m FabricTagId
currentPointer = do pile <- lift $ use (dagFabricStorage . fabricTagPile)
                    return $ FabricTagId (view pileCursor pile - 1)

setTop :: (DagConstraints s m) => SerialMonad s m ()
setTop = do codePointer <- currentPointer
            tSPrimTop .= PTag codePointer

transBoundary :: Space s => FTransformer s -> Box s -> Box s
transBoundary trans box =
  let boxPoints = boxToV4Points box
  in  case trans of
          FAffine ray back -> boxOf $ fmap (applyAffine back) boxPoints
          FFacet facet -> boxOf $ facet ^. facetInput
          FConvolve scale -> addMarginsBox scale box

addShapeToPrims :: (DagConstraints s m) => WithBox (Shape s) -> SerialMonad s m (Slice PrimTagId, Maybe (Box s))
addShapeToPrims (WithBox shape box) =
    do codePointer <- currentPointer
       let outlines = view shapeOutlines . mapOverPoints (fmap clampReasonable) $ shape
           prims = V.concat .
                   map (V.map (Prim codePointer . PrimBezier) .
                        view outlineSegments
                       ) $
                       outlines
           boundingBox = minMaxBoxes . fmap boxOf $ prims
       --liftIO . putStrLn $ "add " ++ show (length prims) ++ " prims " ++ show codePointer
       primTagIds <- lift $ inTree $ V.mapM addTreePrim prims
       primSlice <- lift $ foldIntoPileS dagPrimTagIds primTagIds
       holdPrims primSlice
       tSBox .= Just boundingBox
       return (primSlice, Just boundingBox)

addBoxPrim :: (DagConstraints s m) => FabricTagId -> Box s -> DagMonad s m (Slice PrimTagId)
addBoxPrim fabricTagId box =
    do primTagId <- inTree $ addTreePrim (Prim fabricTagId $ PrimRect box)
       primSlice <- foldIntoPileS dagPrimTagIds [primTagId]
       return primSlice

holdPrims :: (DagConstraints s m) => Slice PrimTagId -> SerialMonad s m ()
holdPrims slice =
    do  top <- use tSPrimTop
        mBox <- use tSBox
        newTop <- case top of
                      PTag tagId     ->
                          case mBox of
                              Just box -> do boxSlice <- lift $ addBoxPrim tagId box
                                             return $ PSlice [boxSlice, slice]
                              Nothing -> return $ PSlice [slice]
                      PSlice slices  -> return $ PSlice (slice:slices)
        tSPrimTop .= newTop

addTreeTags :: (DagConstraints s m) => DecoTagId s -> ConfineTagId s -> SerialMonad s m ()
addTreeTags decoId confineId =
    do addFabricTagS (makeFabTagConfineTree confineId)
       addFabricTagS (makeFabTagDecoTree    decoId   )

makeTree :: (DagConstraints s m) => s -> Int -> SerialMonad s m ()
makeTree limit decorateLimit =
  do top <- use tSPrimTop
     --liftIO . putStrLn $ "makeTree top " ++ show top
     case top of
         PTag _ ->
             do  codePointer <- currentPointer
                 tSPrimTop .= PTag codePointer
         PSlice slices ->
             do  let primSlice = foldl1 combineSlices slices
                 when (sliceLength primSlice > 0) $
                      do (decoTagId, confineTagId) <- lift $ addTreeS limit decorateLimit primSlice
                         addTreeTags decoTagId confineTagId
                         return ()
                 codePointer <- currentPointer
                 tSPrimTop .= PTag codePointer

addFabricTagS :: (DagConstraints s m) => FabricTag -> SerialMonad s m ()
addFabricTagS tag = do lift $ overStateT dagFabricStorage $ addFabricTag tag
                       return ()

addReturnTag    :: (DagConstraints s m) =>                   SerialMonad s m ()
addStackerTag   :: (DagConstraints s m) => FabricTagId    -> SerialMonad s m ()
addBinaryTag    :: (DagConstraints s m) => FCombineType   -> SerialMonad s m ()
addFilterTag    :: (DagConstraints s m) => FFilter        -> SerialMonad s m ()
addTransformTag :: (DagConstraints s m) => FTransformer s -> SerialMonad s m ()
addReturnTag  = addFabricTagS   makeFabTagReturn
addStackerTag = addFabricTagS . makeFabTagStacker
addBinaryTag  = addFabricTagS . combineTypeToFabTag
addFilterTag  = addFabricTagS . makeFilterTag
addTransformTag trans = do tag <- lift $ overStateT dagFabricStorage $ storeTransform trans
                           addFabricTagS tag

addSubstanceTag :: ( DagConstraints (SpaceOf i) m
                   , Storable (FTex i)
                   , Storable (FQuery i)
                   )
                => FSubstance i
                -> SerialMonad (SpaceOf (FSubstance i)) m ()
addSubstanceTag substance = do tag <- lift $ overStateT (dagFabricStorage . fabricHeapPile) $ storeSubstance substance
                               addFabricTagS tag
