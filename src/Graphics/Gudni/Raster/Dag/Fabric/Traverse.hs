{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.Dag.Fabric.Traverse
  ( traverseFabric
  , loadFabricT
  , loadPrimT
  , loadTreeT
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
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.TextureReference

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

data StackRange =
     StackRange
     { _srStack :: V.Vector ShapeId
     , _srMinIndex :: Int
     , _srMaxIndex :: Int
     } deriving (Generic)
makeLenses ''StackRange

srLimits :: String -> StackRange -> (ShapeId, ShapeId)
srLimits message s = ( checkLookup (message ++ "min") (s ^. srStack) (s ^. srMinIndex)
                     , checkLookup (message ++ "max") (s ^. srStack) (s ^. srMaxIndex)
                     )

instance Show StackRange where
   show stack = show (stack ^. srStack) ++ " limits " ++
                (if V.null (stack ^. srStack)
                then "emptyStackRange"
                else show (srLimits "showLimits" stack) )
                -- ++ " minIndex " ++ show (stack ^. srMinIndex)
                -- ++ " maxIndex " ++ show (stack ^. srMaxIndex)

makeStackRange :: ShapeStack
               -> Maybe StackRange
makeStackRange shapeStack =
  if null shapeStack
  then Nothing
  else let shapeIds = V.fromList shapeStack
       in  Just $
           StackRange
           { _srStack = shapeIds
           , _srMinIndex = 0
           , _srMaxIndex = V.length shapeIds - 1
           }

shapesInRange :: Maybe StackRange -> ShapeId -> ShapeId -> Bool
shapesInRange mStackRange rangeMin rangeMax =
  let mLimits = fmap (srLimits "shapesInRange") mStackRange
  in  case mLimits of
          Nothing -> True
          Just (mn, mx) ->
              rangeMin == nullShapeId ||
              (rangeMin <= mn && rangeMax >= mx)

checkLookup :: String -> V.Vector a -> Int -> a
checkLookup message vector i =
   if V.null vector
   then error message
   else (V.!) vector i

findGo :: String
       -> (Int -> Int)
       -> (Int -> Bool)
       -> (a -> Bool)
       -> V.Vector a
       -> Int
       -> Int
findGo message f limit cond vector i = go i
    where
    go i =
        let x = checkLookup message vector i
        in  if limit i || cond x
            then i
            else go (f i)

findUp :: (a -> Bool)
         -> V.Vector a
         -> Int
         -> Int
findUp cond vector i = findGo "up" (+1) (>= V.length vector - 1) cond vector i

findDown :: (a -> Bool)
         -> V.Vector a
         -> Int
         -> Int
findDown cond vector i = findGo "down" (subtract 1) (<= 0) cond vector i

trimRangeMin :: ShapeId -> Maybe StackRange -> Maybe StackRange
trimRangeMin cutPoint =
  fmap (\ stackRange ->
          let newMin = findUp (>= cutPoint)
                       (stackRange ^. srStack   )
                       (stackRange ^. srMinIndex)
          in  set srMinIndex newMin stackRange
       )

trimRangeMax :: ShapeId -> Maybe StackRange -> Maybe StackRange
trimRangeMax cutPoint =
  fmap (\ stackRange ->
          let newMax = findDown (<= cutPoint)
                                (stackRange ^. srStack   )
                                (stackRange ^. srMaxIndex)
          in  set srMaxIndex newMax stackRange
       )

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
traverseFabric ray fabricTagId =
  do liftIO . putStrLn $ "traverseFabric " ++ show ray
     go ray Nothing 0 nullShapeId fabricTagId
     where
     go :: ray
        -> Maybe StackRange
        -> ShapeId
        -> ShapeId
        -> FabricTagId
        -> RayMonad (SpaceOf ray) m q
     go ray mStackRange rangeMin rangeMax fabricTagId =
         do -- liftIO . putStrLn $ "    " ++ show fabricTagId ++ " in ray " ++ show ray ++ " stack " ++ show mStackRange
            tr ("    " ++ show fabricTagId ++ " result ") <$>
                if fabricTagId == nullFabricTagId
                then return .
                     trP ("    " ++ show fabricTagId ++ " shape node stack " ++ show mStackRange) $
                     case mStackRange of
                         Nothing -> emptyQuery
                         Just x  -> insideShape
                else do (WithParent parent fabric) <- loadFabricT fabricTagId
                        liftIO . putStrLn $ "    " ++ show fabricTagId ++ " go ray " ++ show ray ++ " stack " ++ show mStackRange
                                         ++ " r: " ++ show rangeMin ++ "-"   ++ show rangeMax
                                         ++ " fabric " ++ show fabric
                        case fabric of
                            FCombine (op, shapeMinA, midCut) aboveId belowId ->
                                do let stackA = trimRangeMax midCut mStackRange
                                       stackB = trimRangeMin midCut mStackRange
                                   aboveQ <- if tr ("    " ++ show fabricTagId ++ " shapesInRange above    stackA: " ++ show stackA ++ " r: " ++ show rangeMin ++ "-" ++ show midCut) $
                                                shapesInRange stackA rangeMin midCut
                                             then go ray stackA rangeMin midCut aboveId
                                             else return emptyQuery
                                   if traverseStop op aboveQ -- attempt to short circuit the combination based on the first argument.
                                   then return aboveQ
                                   else do belowQ <- if tr ("    " ++ show fabricTagId ++ " shapesInRange below    stackB: " ++ show stackB ++ " r: " ++ show midCut ++ "-" ++ show rangeMax) $
                                                        shapesInRange stackB midCut rangeMax
                                                     then go ray stackB midCut rangeMax belowId
                                                     else return emptyQuery
                                           return $ traverseCombine op aboveQ belowQ
                            FTransform trans childId ->
                                let ray' = overTransform trans ray
                                in  go ray' mStackRange rangeMin rangeMax childId -- should we keep the stack range here. Yes probably.
                            FLeaf leaf ->
                                case leaf of
                                    FTree tree child ->
                                        do shapeStack <- traverseTree ray tree
                                           let shapeStackRange = makeStackRange shapeStack
                                           liftIO . putStrLn $ "    " ++ show fabricTagId ++ " tree " ++ show shapeStackRange
                                           go ray shapeStackRange 0 nullShapeId child
                                    FTreeSubstance substance ->
                                        fromSubstance substance (rayToPoint ray)

traverseTree :: ( MonadIO m
                , Ray ray
                )
             => ray
             -> ConfineTreeId
             -> RayMonad (SpaceOf ray) m ShapeStack
traverseTree ray confineTreeId =
    do  (confineTree, decoTree) <- loadTreeT confineTreeId
        overTree ({-trP "confineTree"-} confineTree) ({-trP "decoTree"-} decoTree) ray


loadFabricT :: (DagConstraints s m) => FabricTagId   -> RayMonad s m (WithParent (ForStorage s))
loadPrimT   :: (DagConstraints s m) => PrimTagId     -> RayMonad s m (Primitive s)
loadTreeT   :: (DagConstraints s m) => ConfineTreeId -> RayMonad s m (ConfineTree s, DecorateTree s)
loadFabricT = lift . loadFabricS
loadPrimT   = lift . loadPrimS
loadTreeT   = lift . loadTreeS
