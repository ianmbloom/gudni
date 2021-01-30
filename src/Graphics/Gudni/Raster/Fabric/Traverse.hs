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

module Graphics.Gudni.Raster.Fabric.Traverse
  ( traverseFabric
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Tag
import Graphics.Gudni.Raster.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.Fabric.Ray.Class
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Combine.Tag
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.Storage
import Graphics.Gudni.Raster.State
import Graphics.Gudni.Raster.WithSerialized
import Graphics.Gudni.Raster.Constants
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
     { _tSFabricStack  :: [FabricItem ray]
     , _tSAnswerStack  :: [q]
     , _tSRay          :: ray
     , _tSCodePointer  :: FabricTagId
     }
makeLenses ''TraverseState

instance (Out ray) => Out (FabricItem ray) where
    doc (Item tagId ray) = doc tagId <+> doc ray
    docPrec _ = doc

instance (Out ray, Out q) => Out (TraverseState ray q) where
    doc state =
        hang (text "FabricStack") 4 (vcat $ map doc (state ^. tSFabricStack))
        $$
        hang (text "AnswerStack") 4 (vcat $ map doc (state ^. tSAnswerStack))
    docPrec _ = doc

initTraverseState :: ray -> FabricTagId -> TraverseState ray q
initTraverseState initRay tagId = TraverseState [] [] initRay tagId

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
pushAnswer = pushStack tSAnswerStack
popAnswer  = do isEmpty <- emptyStack tSAnswerStack
                if isEmpty
                then return outsideShape
                else popStack "tSAnswerStack" tSAnswerStack

emptyFabric :: Monad m => StateT (TraverseState ray q) m Bool
emptyFabric = emptyStack tSFabricStack

pushFabric :: ( Eq ray
              , MonadIO m
              )
           => StateT (TraverseState ray q) m ()
pushFabric =
    do ray <- use tSRay
       code <- use tSCodePointer
       pushStack tSFabricStack (Item ray code)

popFabric :: MonadIO m
          => StateT (TraverseState ray q) m ()
popFabric =
    do (Item ray code)  <- popStack "tSFabricStack" tSFabricStack
       tSRay .= ray
       tSCodePointer .= code

overShapeStack :: MonadIO m => ([FabricItem ray] -> m [FabricItem ray]) -> StateT (TraverseState ray q) m ()
overShapeStack f =
    do stack <- use tSFabricStack
       --liftIO . putStrLn $ "stack full " ++ show stack
       newStack <- lift $ f stack
       --liftIO . putStrLn $ "newStack full " ++ show newStack
       tSFabricStack .= newStack

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
      do -- showState "before"
         fabricTag <- loadNextInstruction
         if fabTagIsReturn fabricTag
         then do isEmpty <- emptyFabric
                 if not isEmpty
                 then do showState "return"
                         popFabric
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
                                       pushAnswer (applyCombine combineType aboveColor belowColor)
        | fabTagIsDecoTree    tag = do tSCodePointer -= 1
                                       confineTag <- loadNextInstruction
                                       let decoId    = fabTagDecoId    tag
                                       let confineId = fabTagConfineId confineTag
                                       ray <- use tSRay
                                       stack <- lift $ rayTraverseTree limit decoId confineId ray []
                                       overShapeStack (return . (stack ++))
        | fabTagIsConfineTree tag = error "decoTag must come first"
        | fabTagIsStacker     tag = do ray <- use tSRay
                                       insertStack (fabTagStackerId tag) ray
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

insertStack :: (MonadIO m, Eq ray) => FabricTagId -> ray -> StateT (TraverseState ray q) m ()
insertStack tagId ray = overShapeStack (return . (Item ray tagId:))

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
