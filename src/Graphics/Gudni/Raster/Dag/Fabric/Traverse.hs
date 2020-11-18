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

-- import Prelude hiding (minimum, maximum)

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.Serialize
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

newtype Stack
    = Stack
    { _unStack :: V.Vector ShapeId
    } deriving (Generic)
makeLenses ''Stack

instance Show Stack where
  show = show . view unStack

stackLookup :: Stack -> StackIndex -> ShapeId
stackLookup stack i = (V.!) (stack ^. unStack) (unStackIndex i)

newtype StackIndex = StackIndex {unStackIndex :: Int} deriving (Eq, Ord, Num)

instance Show StackIndex where
  show = show . unStackIndex

data StackRange
    = StackRange
    { rangeMin :: StackIndex
    , rangeMax :: StackIndex
    }

instance Show StackRange where
  show (StackRange mn mx) = "StackRange " ++ show mn ++ " " ++ show mx

hasRange :: Maybe StackRange
         -> Bool
hasRange mRange =
    case mRange of
        Just range -> rangeMax range > rangeMin range
        Nothing    -> True

makeStackRange :: Stack -> Maybe StackRange
makeStackRange stack = Just $ StackRange 0 (StackIndex . V.length . view unStack $ stack)

data Stage = FirstStage | SecondStage | ThirdStage deriving (Show)

data TraverseState ray q =
     TraverseState
     { _tsShapeStacks  :: [Stack]
     , _tsFabricStack  :: [(FabricTagId, Maybe StackRange, Stage, ray)]
     , _tsColorStack   :: [q]
     }
makeLenses ''TraverseState

initTraverseState :: TraverseState ray q
initTraverseState = TraverseState [] [] []

findSubt :: Stack -> ShapeId -> StackIndex -> StackIndex
findSubt stack cutPoint = go
    where
    go :: StackIndex -> StackIndex
    go i =
        let x = stackLookup stack (i - 1)
        in  if i > 0 && x >= cutPoint
            then go (i - 1)
            else i

pushStack :: (MonadState g m) => Lens' g [a] -> a -> m ()
pushStack lens item = lens %= (item:)

popStack :: (MonadState g m) => String -> Lens' g [a] -> m a
popStack mess lens =
  do list <- use lens
     case list of
       a:rest -> do lens .= rest
                    return a
       []     -> error $ "pop " ++ mess ++ " emptyStack"

peekStack :: (MonadState g m) => Lens' g [a] -> m a
peekStack lens =
  do list <- use lens
     case list of
       a:rest -> return a
       []     -> error "peek emptyStack"

pushFabric :: MonadIO m
           => FabricTagId
           -> Maybe StackRange
           -> Stage
           -> ray
           -> StateT (TraverseState ray q) m ()
pushFabric fabricTagId range stage ray =
    do pushStack tsFabricStack (fabricTagId, range, stage, ray)
       --liftIO $ putStrLn $ "pushFabric " ++ show fabricTagId ++ " " ++ show range ++ " " ++ show stage

popFabric :: MonadIO m
          => StateT (TraverseState ray q) m (FabricTagId, Maybe StackRange, Stage, ray)
popFabric =
    do (fabricTagId, range, stage, ray) <- popStack "popFabric" tsFabricStack
       --liftIO $ putStrLn $ "popFabric " ++ show fabricTagId ++ " " ++ show range ++ " " ++ show stage
       return (fabricTagId, range, stage, ray)

emptyFabric :: Monad m
            => StateT (TraverseState ray q) m Bool
emptyFabric = null <$> use tsFabricStack

emptyShapeStack :: Monad m
                => StateT (TraverseState ray q) m Bool
emptyShapeStack = null <$> use tsShapeStacks

checkShapeStack :: MonadIO m => StateT (TraverseState ray q) m ()
checkShapeStack =
   do shapeEmpty <- emptyShapeStack
      if shapeEmpty
      then liftIO $ putStrLn $ "shapeStack empty"
      else do stackTemp <- peekShapeStack "check"
              liftIO $ putStrLn $ "shapeStack " ++ show stackTemp

fstOf3 (a, _, _) = a
checkFabricStack :: (MonadIO m, HasSpace ray,  Storable (SpaceOf ray), Show ray)
                 => StateT (TraverseState ray q) (RayMonad (SpaceOf ray) m) ()
checkFabricStack = do stack <- use tsFabricStack
                      imapM_ (\i (tagId, mRange, fabricStage, ray ) -> do tag <- loadFabricTagT tagId
                                                                          liftIO $ putStrLn $
                                                                                   show i ++
                                                                                   "     " ++ show tagId ++
                                                                                   " tagX " ++ show tag ++
                                                                                   " range " ++ show mRange ++
                                                                                   " " ++ show fabricStage ++
                                                                                   " " ++ show ray) stack

{-
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
               => ray
               -> FabricTagId
               -> RayMonad (SpaceOf ray) m q
traverseFabric initRay fabricTagId =
    evalStateT (
        do -- liftIO $ putStrLn $ "initRay " ++ show initRay
           pushFabric fabricTagId Nothing FirstStage initRay
           whileM_ (not <$> emptyFabric) $
               do  (tagId, mRange, fabricStage, ray) <- popFabric
                   if tagId == nullFabricTagId
                   then do --liftIO $ putStrLn $ show tagId ++ " range " ++ show mRange ++ " hasRange mRange " ++ show (hasRange mRange)
                           pushColor (if hasRange mRange then insideShape else emptyQuery)
                   else
                       do  tag <- loadFabricTagT tagId
                           --liftIO $ putStrLn $ show tagId ++ " tagX " ++ show tag ++ " range " ++ show mRange ++ " " ++ show fabricStage
                           -- checkFabricStack
                           -- checkShapeStack
                           let traverseTag
                                | fabTagIsLeaf      tag = do substance <- loadSubstanceT tag
                                                             color <- lift $ fromSubstance substance (rayToPoint ray)
                                                             --liftIO $ putStrLn $ "ray " ++ show ray ++ " color " ++ show color
                                                             pushColor color
                                | fabTagIsUnaryPre  tag = if fabTagIsTree tag
                                                          then case fabricStage of
                                                                   FirstStage ->
                                                                       do (confineTreeId, decoTreeId) <- loadTreeRootT (fabTagTreeId tag)
                                                                          newStack <- Stack . V.fromList <$> (lift $ overTree confineTreeId decoTreeId ray)
                                                                          pushShapeStack newStack
                                                                          let newRange = makeStackRange newStack
                                                                          pushFabric (fabTagChildId tag) newRange FirstStage ray
                                                          else -- must be transformer
                                                               case fabricStage of
                                                                        FirstStage ->
                                                                            do transform <- loadTransformT tag
                                                                               let transformedRay = overTransform transform ray
                                                                               pushFabric (fabTagChildId tag) Nothing FirstStage  transformedRay
                                | fabTagIsUnaryPost tag = case fabricStage of
                                                              FirstStage ->
                                                                  do pushFabric  tagId              Nothing SecondStage ray
                                                                     pushFabric (fabTagChildId tag) mRange  FirstStage  ray
                                                              SecondStage ->
                                                                  do color <- popColor
                                                                     filt <- loadFilterT tag
                                                                     let filteredColor = applyFilter filt color
                                                                     pushColor filteredColor
                                | fabTagIsBinaryOp  tag = do  limits <- loadFabricLimT tagId
                                                              case fabricStage of
                                                                  FirstStage ->
                                                                      do (aboveRange, belowRange) <-
                                                                             case mRange of
                                                                                 Nothing    -> return ( Nothing
                                                                                                      , Nothing
                                                                                                      )
                                                                                 Just range -> if snd limits == nullShapeId
                                                                                               then return (mRange, mRange)
                                                                                               else  do stack <- peekShapeStack "getLimits"
                                                                                                        let cutIndex = findSubt stack (snd limits) (rangeMax range)
                                                                                                        return ( Just (StackRange (rangeMin range) cutIndex)
                                                                                                               , Just (StackRange cutIndex (rangeMax range))
                                                                                                               )
                                                                         --liftIO $ putStrLn $ "snd limits " ++ show (snd limits) ++ " aboveRange " ++ show aboveRange ++ " belowRange " ++ show belowRange
                                                                         pushFabric tagId belowRange SecondStage ray
                                                                         if hasRange aboveRange
                                                                         then pushFabric (fabTagAboveId tag) aboveRange FirstStage ray
                                                                         else pushColor emptyQuery
                                                                  SecondStage ->
                                                                      do aboveQ <- popColor
                                                                         let combineType = fabTagCombineType tag
                                                                         if traverseStop combineType aboveQ
                                                                         then do pushColor aboveQ
                                                                                 return ()
                                                                         else do pushColor aboveQ
                                                                                 pushFabric tagId Nothing ThirdStage ray
                                                                                 if hasRange mRange
                                                                                 then pushFabric (fabTagBelowId tag) mRange FirstStage ray
                                                                                 else pushColor emptyQuery
                                                                  ThirdStage ->
                                                                      do belowQ <- popColor
                                                                         aboveQ <- popColor
                                                                         let combineType = fabTagCombineType tag
                                                                             color = traverseCombine combineType aboveQ belowQ
                                                                         --liftIO $ putStrLn $ "combineType " ++ show combineType ++ " aboveQ " ++ show aboveQ ++ " belowQ " ++ show belowQ ++ " color " ++ show color
                                                                         pushColor color
                           traverseTag
                   return () -- end whileM
           color <- popColor
           --liftIO $ putStrLn $ "done loop " ++ show initRay ++ " -> " ++ show color
           return color
    ) initTraverseState
-}

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
               => ray
               -> FabricTagId
               -> RayMonad (SpaceOf ray) m q
traverseFabric initRay fabricTagId =
    evalStateT (go fabricTagId Nothing FirstStage initRay ) initTraverseState
    where
    popGo :: StateT (TraverseState ray q) (RayMonad (SpaceOf ray) m) q
    popGo = do isEmpty <- emptyFabric
               if isEmpty
               then popColor
               else do (tagId, mRange, fabricStage, ray) <- popFabric
                       go tagId mRange fabricStage ray
    go :: FabricTagId -> Maybe StackRange -> Stage -> ray -> StateT (TraverseState ray q) (RayMonad (SpaceOf ray) m) q
    go tagId mRange fabricStage ray =
       if tagId == nullFabricTagId
       then do --liftIO $ putStrLn $ show tagId ++ " range " ++ show mRange ++ " hasRange mRange " ++ show (hasRange mRange)
               pushColor (if hasRange mRange then insideShape else emptyQuery)
               popGo
       else
           do  tag <- loadFabricTagT tagId
               --liftIO $ putStrLn $ show tagId ++ " tagX " ++ show tag ++ " range " ++ show mRange ++ " " ++ show fabricStage
               -- checkFabricStack
               -- checkShapeStack
               let traverseTag
                    | fabTagIsLeaf      tag = do substance <- loadSubstanceT tag
                                                 color <- lift $ fromSubstance substance (rayToPoint ray)
                                                 --liftIO $ putStrLn $ "ray " ++ show ray ++ " color " ++ show color
                                                 pushColor color
                                                 popGo
                    | fabTagIsUnaryPre  tag = if fabTagIsTree tag
                                              then case fabricStage of
                                                       FirstStage ->
                                                           do (confineTreeId, decoTreeId) <- loadTreeRootT (fabTagTreeId tag)
                                                              newStack <- Stack . V.fromList <$> (lift $ overTree confineTreeId decoTreeId ray)
                                                              pushShapeStack newStack
                                                              let newRange = makeStackRange newStack
                                                              go (fabTagChildId tag) newRange FirstStage ray
                                              else -- must be transformer
                                                   case fabricStage of
                                                            FirstStage ->
                                                                do transform <- loadTransformT tag
                                                                   let transformedRay = overTransform transform ray
                                                                   go (fabTagChildId tag) Nothing FirstStage  transformedRay
                    | fabTagIsUnaryPost tag = case fabricStage of
                                                  FirstStage ->
                                                      do pushFabric  tagId              Nothing SecondStage ray
                                                         go (fabTagChildId tag) mRange  FirstStage  ray
                                                  SecondStage ->
                                                      do color <- popColor
                                                         filt <- loadFilterT tag
                                                         let filteredColor = applyFilter filt color
                                                         pushColor filteredColor
                                                         popGo
                    | fabTagIsBinaryOp  tag = do  limits <- loadFabricLimT tagId
                                                  case fabricStage of
                                                      FirstStage ->
                                                          do (aboveRange, belowRange) <-
                                                                 case mRange of
                                                                     Nothing    -> return ( Nothing
                                                                                          , Nothing
                                                                                          )
                                                                     Just range -> if snd limits == nullShapeId
                                                                                   then return (mRange, mRange)
                                                                                   else  do stack <- peekShapeStack "getLimits"
                                                                                            let cutIndex = findSubt stack (snd limits) (rangeMax range)
                                                                                            return ( Just (StackRange (rangeMin range) cutIndex)
                                                                                                   , Just (StackRange cutIndex (rangeMax range))
                                                                                                   )
                                                             --liftIO $ putStrLn $ "snd limits " ++ show (snd limits) ++ " aboveRange " ++ show aboveRange ++ " belowRange " ++ show belowRange
                                                             if hasRange aboveRange
                                                             then do pushFabric tagId belowRange SecondStage ray
                                                                     go (fabTagAboveId tag) aboveRange FirstStage ray
                                                             else do pushColor emptyQuery
                                                                     go tagId belowRange SecondStage ray
                                                      SecondStage ->
                                                          do aboveQ <- popColor
                                                             let combineType = fabTagCombineType tag
                                                             if traverseStop combineType aboveQ
                                                             then do pushColor aboveQ
                                                                     popGo
                                                             else do pushColor aboveQ
                                                                     if hasRange mRange
                                                                     then do pushFabric tagId Nothing ThirdStage ray
                                                                             go (fabTagBelowId tag) mRange FirstStage ray
                                                                     else do pushColor emptyQuery
                                                                             go tagId Nothing ThirdStage ray
                                                      ThirdStage ->
                                                          do belowQ <- popColor
                                                             aboveQ <- popColor
                                                             let combineType = fabTagCombineType tag
                                                                 color = traverseCombine combineType aboveQ belowQ
                                                             --liftIO $ putStrLn $ "combineType " ++ show combineType ++ " aboveQ " ++ show aboveQ ++ " belowQ " ++ show belowQ ++ " color " ++ show color
                                                             pushColor color
                                                             popGo
               traverseTag

loadFabricT    :: (DagConstraints s m) => FabricTagId        -> StateT t (RayMonad s m) (Fabric (ForStorage s))
loadTransformT :: (DagConstraints s m) => FabricTag          -> StateT t (RayMonad s m) (FTransformer s)
loadFilterT    :: (DagConstraints s m) => FabricTag          -> StateT t (RayMonad s m) FFilter
loadSubstanceT :: (DagConstraints s m) => FabricTag          -> StateT t (RayMonad s m) (FSubstance (ForStorage s))
loadFabricTagT :: (DagConstraints s m) => FabricTagId        -> StateT t (RayMonad s m) FabricTag
loadFabricLimT :: (DagConstraints s m) => FabricTagId        -> StateT t (RayMonad s m) (ShapeId, ShapeId)
loadPrimT      :: (DagConstraints s m) => PrimTagId          -> StateT t (RayMonad s m) (Primitive s)
loadTreeRootT  :: (DagConstraints s m) => Reference (Root s) -> StateT t (RayMonad s m) (Root s)
loadTreeTagT   :: (DagConstraints s m) => ConfineTagId s     -> StateT t (RayMonad s m) (ConfineTag s)
loadDecoTagT   :: (DagConstraints s m) => DecoTagId s        -> StateT t (RayMonad s m) (DecoTag s)
loadFabricT    = lift . lift . loadFabricS
loadTransformT = lift . lift . loadTransformS
loadFilterT    = lift . lift . loadFilterS
loadSubstanceT = lift . lift . loadSubstanceS
loadFabricTagT = lift . lift . loadFabricTagS
loadFabricLimT = lift . lift . loadFabricLimS
loadPrimT      = lift . lift . loadPrimS
loadTreeRootT  = lift . lift . loadTreeRootS
loadTreeTagT   = lift . lift . loadTreeTagS
loadDecoTagT   = lift . lift . loadDecoTagS

pushColor      :: (Show q, MonadIO m) => q     -> StateT (TraverseState ray q) m ()
popColor       :: (Show q, MonadIO m) => StateT (TraverseState ray q) m q
pushShapeStack :: MonadIO m => Stack -> StateT (TraverseState ray q) m ()
popShapeStack  :: MonadIO m => StateT (TraverseState ray q) m Stack
peekShapeStack :: MonadIO m => String -> StateT (TraverseState ray q) m Stack
pushColor color = do --liftIO $ putStrLn $ "pushColor" ++ show color
                     pushStack tsColorStack color
popColor       = do  color <- popStack  "popColor"      tsColorStack
                     --liftIO $ putStrLn $ "popColor" ++ show color
                     return color
pushShapeStack = pushStack                 tsShapeStacks
popShapeStack  = popStack  "popShapeStack" tsShapeStacks
peekShapeStack mess = do --liftIO $ putStrLn $ "peekShapeStack " ++ mess
                         peekStack                 tsShapeStacks
