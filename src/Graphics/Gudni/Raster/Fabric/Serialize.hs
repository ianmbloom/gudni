<<<<<<< HEAD
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
=======
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
>>>>>>> origin/flatpath

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

<<<<<<< HEAD
=======
import Graphics.Gudni.Base
>>>>>>> origin/flatpath
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
<<<<<<< HEAD

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
=======
import Graphics.Gudni.Layout.Font

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Storage
>>>>>>> origin/flatpath
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
<<<<<<< HEAD
-- import Graphics.Gudni.Raster.FromLayout
=======
import Graphics.Gudni.Raster.Fabric.FromLayout
>>>>>>> origin/flatpath
import Graphics.Gudni.Raster.Storage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Lens
import Foreign.Storable

<<<<<<< HEAD
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
=======
import qualified Data.Vector     as V
import qualified Data.Map.Strict as M

data PrimTop s
    = PTag FabricTagId
    | PSlice [Slice PrimTagId]
    deriving (Show)

data SerialState i glyphType
    = SerialState
    { _srVarMap  :: M.Map (FVarName i) (FabricTagId, Maybe (Box (SpaceOf i)))
    , _srGlyphMap:: M.Map glyphType (FabricTagId, Maybe (Box (SpaceOf i)))
    , _srDepth :: Int
    }
makeLenses ''SerialState

type SerialMonad i glyphType m a = StateT (SerialState i glyphType) (DagMonad (SpaceOf i) m) a

primsFromSlice :: ( MonadIO m, Storable s, Space s ) => Slice PrimTagId -> DagMonad s m [(PrimTagId, FabricTagId)]
primsFromSlice =
    mapSliceM $
        \ i -> do primId <- fromPileS dagPrimTagIds i
                  prim   <- overStateT dagTreeStorage (loadTreePrim primId)
                  return (primId, prim ^. primFabricTagId)

>>>>>>> origin/flatpath

announce :: MonadIO m => String -> m ()
announce mess = liftIO $ putStrLn $ "===================== Serialize Fabric " ++ mess ++ " ====================="

<<<<<<< HEAD
serializeFabric :: forall i m style
                .  ( DagConstraints (SpaceOf style) m
                   , IsStyle style
                   , SpaceOf (i style) ~ SpaceOf style
                   , FBinaryType (i style) ~ ProximityMeld style FCombineType
                   , FChildType (i style) ~ Fabric (i style)
                   , FLeafType (i style) ~ FLeaf (i style)
                   , FPreType (i style) ~ FTransformer (SpaceOf style)
                   , FPostType (i style) ~ FFilter
                   , Answer (FQuery (i style))
                   , Storable (FTex (i style))
                   , Storable (FQuery (i style))
                   )
                => SpaceOf style
                -> Int
                -> Fabric (i style)
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
    go :: Fabric (i style)
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
                           do  addSubstanceTag (FConst insideShape :: FSubstance (i style))
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
=======
atDepth :: (DagConstraints (SpaceOf i) m) => String -> SerialMonad i glyphType m ()
atDepth mess =
    do  depth <- use srDepth
        liftIO . putStrLn $ (replicate (depth * 4) ' ') ++ show depth ++ mess

insertLookup kx x t = M.insertLookupWithKey (\_ a _ -> a) kx x t

data ShapeDependant i
     = Dependant (Maybe (WithBox (Shape (SpaceOf i)))) (Fabric i)
     | Encoded ([Slice PrimTagId], [FabricTagId])

encodeFabric :: DagConstraints (SpaceOf i) m
             => Fabric i -> SerialMonad i glyphType m (FabricTagId)
encodeFabric fabric =
   do go fabric
      currentPointer
   where
   go fabric =
       case fabric of
           FBinary ty above below ->
               do  go above
                   go below
                   let melder = ty ^. proxMeld
                   addBinaryTag melder
           FUnaryPost post child ->
               do  addFilterTag post
                   go child
           FUnaryPre pre child ->
               do  go child
                   addTransformTag pre
           FLeaf leaf ->
               case leaf of
                   FShape -> addSubstanceTag (FConst insideShape :: FSubstance i)
                   FSubstance -> addSubstanceTag substance
           FVar  tagId -> addStackerTag tagId
           FDefine {} -> error "FDefine"

type DependentShape s = Maybe (Shape s, Box s)

simpleCombine ty (FLeaf (FSubstance (FConst i))) (FLeaf (FSubstance (FConst j))) =
    FLeaf $ FSubstance $ applyCombine ty i j
simpleCombine ty a b =
    FBinary ty (FLeaf a) (FLeaf b)

combineIsConfined ty =
    case ty of
        FMask -> True
        _     -> False

buildCombine ty aFab bFab =
    do  addReturnTag
        addBinaryTag ty
        encodeFabric bFab
        bJump <- currentPointer
        addReturnTag
        encodeFabric aFab
        aJump <- currentPointer
        return $ (aJump, bJump) (combineBoxes a b)


simplifyFabric :: (s ~ SpaceOf i)
               => Fabric i -> x
simplifyFabric fabric = go
    where
    go :: Fabric i -> (DependentShape s, Either (Fabric j) ([FabricTagId], Maybe (Box s))
    go fabric =
        case fabric of
            FBinary ty above below ->
                do  bChild <- go below
                    aChild <- go above
                    let melder = ty ^. proxMeld
                    combineChildren aChild bChild
            FUnaryPost post child ->
                do  go child
            FUnaryPre pre child ->
                do  go child
            FLeaf leaf ->
                case leaf of
                  FShape     shape     -> (Just shape, Left $ FLeaf (FConst insideShape :: FSubstance i))
                  FSubstance substance -> (Nothing,    Left $ FLeaf substance)
            FVar var -> (Nothing, Left $ FVar var)
            FDefine var body applied ->
               do addReturnTag
                  appliedTop <- go applied
                  withVar var body appliedTop

call :: ( DagConstraints (SpaceOf i) m
        , Show item
        , Ord item
        )
     => item
     -> Lens' (SerialState i glyphType) (M.Map item (Maybe (Box (SpaceOf i)), FabricTagId))
     -> SerialMonad i glyphType m ()
call item itemMapLens =
    do itemMap <- use itemMapLens
       case M.lookup item itemMap of
           Just child -> addStackerTag tagId
           Nothing -> error $ "item not found " ++ show item

withVar :: ( DagConstraints (SpaceOf i) m
           , Ord (FVarName i)
           )
        => FVarName i
        -> (Maybe (Box (SpaceOf i)), Either
        -> SerialMonad i glyphType m ()
        -> SerialMonad i glyphType m ()
withVar varName (mBox, _) code =
    do codePointer <- currentPointer
       varMap <- use srVarMap
       let (mOld, varMap') = insertLookup varName (codePointer, mBox) varMap
       srVarMap .= varMap'
       h <- code
       case mOld of
           Nothing  -> srVarMap %= M.delete varName
           Just old -> srVarMap %= M.insert varName old
       return h

goto :: (DagConstraints (SpaceOf i) m) => (FabricTagId, Maybe (Box (SpaceOf i))) -> SerialMonad i glyphType m (Head (SpaceOf i))
goto (tagId, mBox) =
   do -- currentId <- currentPointer
      addReturnTag
      addStackerTag tagId
      -- addStackerTag currentId
      return ([], mBox)
      --srMBox .= mBox

currentPointer :: (DagConstraints (SpaceOf i) m) => SerialMonad i glyphType m FabricTagId
currentPointer =
     do pile <- lift $ use (dagFabricStorage . fabricTagPile)
        return $ FabricTagId (view pileCursor pile - 1)
>>>>>>> origin/flatpath

transBoundary :: Space s => FTransformer s -> Box s -> Box s
transBoundary trans box =
  let boxPoints = boxToV4Points box
  in  case trans of
          FAffine ray back -> boxOf $ fmap (applyAffine back) boxPoints
          FFacet facet -> boxOf $ facet ^. facetInput
          FConvolve scale -> addMarginsBox scale box

<<<<<<< HEAD
addShapeToPrims :: (DagConstraints s m) => WithBox (Shape s) -> SerialMonad s m (Slice PrimTagId, Maybe (Box s))
=======
addShapeToPrims :: (DagConstraints (SpaceOf i) m)
                => WithBox (Shape (SpaceOf i))
                -> SerialMonad i glyphType m (Head (SpaceOf i))
>>>>>>> origin/flatpath
addShapeToPrims (WithBox shape box) =
    do codePointer <- currentPointer
       let outlines = view shapeOutlines . mapOverPoints (fmap clampReasonable) $ shape
           prims = V.concat .
                   map (V.map (Prim codePointer . PrimBezier) .
                        view outlineSegments
                       ) $
                       outlines
           boundingBox = minMaxBoxes . fmap boxOf $ prims
<<<<<<< HEAD
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
=======
       primTagIds <- lift $ inTree $ V.mapM addTreePrim prims
       primSlice <- lift $ foldIntoPileS dagPrimTagIds primTagIds
       return ([primSlice], [], Just boundingBox)

addBoxToPrims :: (DagConstraints s m)
              => FabricTagId
              -> Box s
              -> DagMonad s m (Slice PrimTagId)
addBoxToPrims fabricTagId box =
    do primTagId <- inTree $ addTreePrim (Prim fabricTagId $ PrimRect box)
       primSlice <- foldIntoPileS dagPrimTagIds [primTagId]
       return $ primSlice

addTreeTags :: (DagConstraints (SpaceOf i) m)
            => DecoTagId (SpaceOf i)
            -> ConfineTagId (SpaceOf i)
            -> SerialMonad i glyphType m ()
>>>>>>> origin/flatpath
addTreeTags decoId confineId =
    do addFabricTagS (makeFabTagConfineTree confineId)
       addFabricTagS (makeFabTagDecoTree    decoId   )

<<<<<<< HEAD
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
=======
makeTree :: (DagConstraints (SpaceOf i) m)
         => SpaceOf i
         -> Int
         -> Head (SpaceOf i)
         -> SerialMonad i glyphType m (Head (SpaceOf i))
makeTree limit decorateLimit (slices, tagIds, mBox) =
  do when (not . null slices) $
          do let primSlice = foldl1 combineSlices slices
             when (sliceLength primSlice > 0) $
                  do addReturnTag
                     (decoTagId, confineTagId) <- lift $ addTreeS limit decorateLimit primSlice
                     addTreeTags decoTagId confineTagId
     mapM_ addStackerTag tagIds
     return ([], [], mBox)


addFabricTagS :: (DagConstraints (SpaceOf i) m) => FabricTag -> SerialMonad i glyphType m ()
addFabricTagS tag = do tagId <- (+1). unFabricTagId <$> currentPointer
                       atDepth $ ", " ++ show tagId ++ " addTag " ++ show tag
                       lift $ overStateT dagFabricStorage $ addFabricTag tag
                       return ()

addReturnTag    :: (DagConstraints (SpaceOf i) m) =>                             SerialMonad i glyphType m ()
addStackerTag   :: (DagConstraints (SpaceOf i) m) => FabricTagId              -> SerialMonad i glyphType m ()
addBinaryTag    :: (DagConstraints (SpaceOf i) m) => FCombineType             -> SerialMonad i glyphType m ()
addFilterTag    :: (DagConstraints (SpaceOf i) m) => FFilter                  -> SerialMonad i glyphType m ()
addTransformTag :: (DagConstraints (SpaceOf i) m) => FTransformer (SpaceOf i) -> SerialMonad i glyphType m ()
>>>>>>> origin/flatpath
addReturnTag  = addFabricTagS   makeFabTagReturn
addStackerTag = addFabricTagS . makeFabTagStacker
addBinaryTag  = addFabricTagS . combineTypeToFabTag
addFilterTag  = addFabricTagS . makeFilterTag
addTransformTag trans = do tag <- lift $ overStateT dagFabricStorage $ storeTransform trans
                           addFabricTagS tag

addSubstanceTag :: ( DagConstraints (SpaceOf i) m
<<<<<<< HEAD
=======
                   , HasSpace i
                   , Storable (SpaceOf i)
>>>>>>> origin/flatpath
                   , Storable (FTex i)
                   , Storable (FQuery i)
                   )
                => FSubstance i
<<<<<<< HEAD
                -> SerialMonad (SpaceOf (FSubstance i)) m ()
=======
                -> SerialMonad i glyphType m ()
>>>>>>> origin/flatpath
addSubstanceTag substance = do tag <- lift $ overStateT (dagFabricStorage . fabricHeapPile) $ storeSubstance substance
                               addFabricTagS tag
