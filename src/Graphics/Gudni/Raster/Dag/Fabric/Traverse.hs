{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.Dag.Fabric.Traverse
  ( traverseFabric
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Dag.Fabric.Filter.Type
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Serial.Reference

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable

import Linear.Vector
import Linear.Metric
import Data.Kind
import Data.Foldable

import qualified Data.Vector as V
import Control.Monad.Loops

data TraverseState ray q =
     TraverseState
     { _tSFabricStack  :: [ShapeStack]
     , _tSRayStack     :: [ray]
     , _tSAnswerStack  :: [q]
     , _tSRay          :: ray
     , _tSCodePointer  :: FabricTagId
     }
makeLenses ''TraverseState

instance (Out ray, Out q) => Out (TraverseState ray q) where
    doc state =
      let stacks = zip (state ^. tSFabricStack) (state ^. tSRayStack)
          docStack (shapeStack, ray) = hang (doc ray) 4 (vcat $ map doc shapeStack)
      in  hang (text "FabricStack") 4 (vcat $ map docStack stacks)
          $$
          hang (text "AnswerStack") 4 (vcat $ map doc (state ^. tSAnswerStack))
    docPrec _ = doc

initTraverseState :: ray -> FabricTagId -> TraverseState ray q
initTraverseState initRay tagId = TraverseState [] [] [] initRay tagId

emptyStack :: (MonadState g m) => Lens' g [a] -> m Bool
emptyStack lens = null <$> use lens

pushStack :: (MonadState g m) => Lens' g [a] -> a -> m ()
pushStack lens item = lens %= (item:)

popStack :: (MonadState g m) => String -> Lens' g [a] -> m a
popStack mess lens =
  do list <- use lens
     case list of
       a:rest -> do lens .= rest
                    return a
       []     -> error $ "pop emptyStack " ++ mess

peekStack :: (MonadState g m) => String -> Lens' g [a] -> m a
peekStack mess lens =
  do list <- use lens
     case list of
       a:rest -> return a
       []     -> error $ "peek emptyStack " ++ mess

pushAnswer :: (Answer q, MonadIO m) => q -> StateT (TraverseState ray q) m ()
popAnswer  :: (Answer q, MonadIO m) =>      StateT (TraverseState ray q) m q
peekAnswer :: (Answer q, MonadIO m) =>      StateT (TraverseState ray q) m q
pushAnswer = pushStack tSAnswerStack
popAnswer  = do isEmpty <- emptyStack tSAnswerStack
                if isEmpty
                then return outsideShape
                else popStack "tSAnswerStack" tSAnswerStack
peekAnswer = do isEmpty <- emptyStack tSAnswerStack
                if isEmpty
                then return outsideShape
                else peekStack "tSAnswerStack" tSAnswerStack

emptyFabric :: Monad m => StateT (TraverseState ray q) m Bool
emptyFabric = emptyStack tSFabricStack

pushFabric :: ( Eq ray
              , MonadIO m
              )
           => StateT (TraverseState ray q) m ()
pushFabric =
    do ray <- use tSRay
       code <- use tSCodePointer
       rayEmpty <- emptyStack tSRayStack
       newRay <- if rayEmpty
                 then return True
                 else do oldRay <- peekStack "tSRayStack" tSRayStack
                         return $ ray == oldRay
       if newRay
       then do pushStack tSFabricStack [code]
               pushStack tSRayStack    ray
       else overShapeStack (return . (code:))

popFabric :: MonadIO m
          => StateT (TraverseState ray q) m ()
popFabric =
    do rayEmpty <- emptyStack tSRayStack
       if rayEmpty
       then error "popFabric empty"
       else do ray  <- popStack "tSRayStack"    tSRayStack
               cs   <- popStack "tSFabricStack" tSFabricStack
               when (not . null . tail $ cs) $
                   do pushStack tSRayStack ray
                      pushStack tSFabricStack (tail cs)
               tSRay .= ray
               tSCodePointer .= head cs

overShapeStack :: MonadIO m => (ShapeStack -> m ShapeStack) -> StateT (TraverseState ray q) m ()
overShapeStack f =
    do rayEmpty <- emptyStack tSRayStack
       if rayEmpty
       then do newStack <- lift $ f []
               --liftIO . putStrLn $ "newStack empty " ++ show newStack
               pushStack tSFabricStack newStack
               ray <- use tSRay
               pushStack tSRayStack ray
       else do stack <- popStack "tSFabricStack overShapeStack" tSFabricStack
               --liftIO . putStrLn $ "stack full " ++ show stack
               newStack <- lift $ f stack
               --liftIO . putStrLn $ "newStack full " ++ show newStack
               pushStack tSFabricStack newStack

traverseFabric :: forall m ray q
               .  ( Show ray
                  , Show q
                  , Out ray
                  , Out q
                  , MonadIO m
                  , Ray ray
                  , Answer q
                  , SpaceOf q ~ SpaceOf ray
                  )
               => SpaceOf ray
               -> ray
               -> ray
               -> RayMonad (SpaceOf ray) m q
traverseFabric limit checkRay initRay =
    do start <- lift fabricCodeStart
       evalStateT (
          do go
             if (initRay == checkRay)
             then return insideShape
             else popAnswer
          ) (initTraverseState initRay start)
    where
    debugIf mess = when (initRay == checkRay) $ liftIO . putStrLn $ mess
    showState :: String -> StateT (TraverseState ray q) (RayMonad (SpaceOf ray) m) ()
    showState mess = do state <- get
                        tag <- loadNextInstruction
                        debugIf $ render $ hang (doc (state ^. tSCodePointer) <+> text (show tag) <+> doc (state ^. tSRay) <+> text mess) 8 (doc state)
    go =
      do showState "before"
         fabricTag <- loadNextInstruction
         if fabTagIsReturn fabricTag
         then do isEmpty <- emptyFabric
                 if not isEmpty
                 then do popFabric
                         go
                 else return ()
         else do marshallTag fabricTag
                 showState "after marshall"
                 tSCodePointer -= 1
                 go
    loadNextInstruction = do cursor <- use tSCodePointer
                             loadFabricTagT cursor
    marshallTag tag
        | fabTagIsConstant    tag = do answer <- constantQueryT tag
                                       debugIf $ "Constant " ++ show answer
                                       pushAnswer answer
        | fabTagIsTexture     tag = do ray <- use tSRay
                                       answer <- textureQueryT tag (rayToPoint ray)
                                       debugIf $ "Texture " ++ show answer
                                       pushAnswer answer
        | fabTagIsFunction    tag = marshallFunction tag
        | fabTagIsBinary      tag = do belowColor <- popAnswer
                                       aboveColor <- popAnswer
                                       let combineType = fabTagCombineType tag
                                       pushAnswer (traverseCombine combineType aboveColor belowColor)
        | fabTagIsDecoTree    tag = do tSCodePointer -= 1
                                       confineTag <- loadNextInstruction
                                       let decoId    = fabTagDecoId    tag
                                       let confineId = fabTagConfineId confineTag
                                       ray <- use tSRay
                                       overShapeStack (rayTraverseTree limit decoId confineId ray)
        | fabTagIsConfineTree tag = error "decoTag must come first"
        | fabTagIsStacker     tag = do insertStack (fabTagStackerId tag)
        | fabTagIsAffine      tag = do affine <- loadAffineT tag
                                       tSRay %= rayApplyTransform limit affine
        | fabTagIsFacet       tag = do facet <- loadFacetT tag
                                       tSRay %= rayApplyTransform limit facet
        | fabTagIsConvolve    tag = do convolve <- loadConvolveT tag
                                       tSRay %= rayApplyTransform limit convolve
    marshallFunction tag
        | fabTagIsLinear      tag = do ray <- use tSRay
                                       let answer = linearQuery (rayToPoint ray)
                                       debugIf $ "Linear " ++ show answer
                                       pushAnswer $ answer
        | fabTagIsQuadrance   tag = do ray <- use tSRay
                                       let answer = quadranceQuery (rayToPoint ray)
                                       debugIf $ "Quadrance " ++ show answer
                                       pushAnswer $ answer

insertStack :: (MonadIO m, Eq ray) => FabricTagId -> StateT (TraverseState ray q) m ()
insertStack tagId = overShapeStack (return . toggleShapeActive tagId)

loadFabricTagT :: (DagConstraints s m) => FabricTagId -> StateT t (RayMonad s m) FabricTag
loadAffineT    :: (DagConstraints s m) => FabricTag   -> StateT t (RayMonad s m) (FTransformer s)
loadFacetT     :: (DagConstraints s m) => FabricTag   -> StateT t (RayMonad s m) (FTransformer s)
loadConvolveT  :: (DagConstraints s m) => FabricTag   -> StateT t (RayMonad s m) (FTransformer s)
loadFabricTagT = lift . lift . loadFabricTagS --
loadAffineT    = lift . lift . loadAffineS    --
loadFacetT     = lift . lift . loadFacetS     --
loadConvolveT  = lift . lift . loadConvolveS  --

constantQueryT :: (DagConstraints (SpaceOf q) m, Answer q) => FabricTag -> StateT t (RayMonad (SpaceOf q) m) q
constantQueryT tag = lift . lift $ constantQuery tag

textureQueryT  :: (DagConstraints (SpaceOf q) m, Answer q) => FabricTag -> Point2 (SpaceOf q) -> StateT t (RayMonad (SpaceOf q) m) q
textureQueryT tag point = lift . lift $ textureQuery tag point
