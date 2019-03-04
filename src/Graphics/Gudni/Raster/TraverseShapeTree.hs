{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Graphics.Gudni.Raster.TraverseShapeTree
  ( EnclosureMonad (..)
  , runEnclosureMonad
  , PrimId (..)
  , makeCurveEnclosure
  , ShapeTreeState (..)
  , stTokenMap
  , stPictureRefs
  , traverseShapeTree
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Raster.ShapeInfo

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.DeepSeq
import Control.Monad.State

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Control.Parallel.Strategies

type EnclosureMonad m = StateT (CurveTable, Int) m

runEnclosureMonad :: (Monad m) => EnclosureMonad m a -> m a
runEnclosureMonad code =
  do let curveTable = buildCurveTable mAXsECTIONsIZE
     (evalStateT code) (curveTable, mAXsECTIONsIZE)

type Combiners t = Group (Combiner t)
type Shapers   t = Group (Shaper t)
type Outlines    = Group (Outline DisplaySpace)

type PrimId = Reference (Shaper Enclosure)

data ShapeTreeState token = ShapeTreeState
    { _stGroupId           :: GroupId
    , _stTokenMap          :: M.Map token GroupId
    , _stCurrentPictureRef :: Int
    , _stPictureRefs       :: [PictureRef PictureMemory]
    , _stPictureMems       :: [PictureMemory]
    }
makeLenses ''ShapeTreeState

excludeBox :: Point2 DisplaySpace
           -> BoundingBox
           -> Bool
excludeBox canvasSize box =
           box ^. leftSide   >= canvasSize ^. pX
        || box ^. topSide    >= canvasSize ^. pY
        || box ^. rightSide  <= 0
        || box ^. bottomSide <= 0

makeCurveEnclosures :: CurveTable
                    -> Int
                    -> Point2 DisplaySpace
                    -> Group (Shaper Outlines)
                    -> [Shaper (BoundingBox, Enclosure)]
makeCurveEnclosures curveTable sectionSize canvasSize  =
    catMaybes . unGroup . fmap (makeCurveEnclosure curveTable sectionSize canvasSize)

makeCurveEnclosure :: CurveTable
                   -> Int
                   -> Point2 DisplaySpace
                   -> Shaper Outlines
                   -> Maybe (Shaper (BoundingBox, Enclosure))
makeCurveEnclosure curveTable sectionSize canvasSize (Shaper shapeInfo outlines) =
     let boundingBox = getBoundingBox outlines
     in  if excludeBox canvasSize boundingBox
         then Nothing
         else let enclosure = enclose curveTable sectionSize (unGroup outlines)
              in  Just (Shaper shapeInfo (boundingBox, enclosure))

type ShapeTreeMonad token = State (ShapeTreeState token)

evalShapeTreeMonad :: [PictureMemory]
                   -> ShapeTreeMonad token a
                   -> (a, ShapeTreeState token)
evalShapeTreeMonad pictureMems mf = runState mf (ShapeTreeState (GroupId 0) M.empty 0 [] pictureMems)

-- | Traverse the ShapeTree while assigning ids to every shapeGroup, recording every tokens relationship to
-- and assigning a picture reference to each referenced picture.
parseShapeTree :: (Ord token)
               =>                       STree overlap trans (SRep token (PictureRef PictId) a)
               -> ShapeTreeMonad token (STree overlap trans (SRep GroupId PictId            a))
parseShapeTree = \case
  SLeaf (SRep token substance compound) ->
      do  shapeId  <- use stGroupId
          stGroupId += 1
          tokenMap <- use stTokenMap
          stTokenMap .= M.insert token shapeId tokenMap
          colorOrPicture <-
             case substance of
                 Texture pictureRef -> do
                     mems <- use stPictureMems
                     let pictId = pictData pictureRef
                         newRef = pictureRef { pictData = mems !! fromIntegral pictId
                                             , pictTranslate = zeroPoint}
                     stPictureRefs %= (newRef:)
                     current <- use stCurrentPictureRef
                     stCurrentPictureRef += 1
                     return . Texture . fromIntegral $ current
                 Solid color -> return $ Solid color
          return $ SLeaf (SRep shapeId colorOrPicture compound)
  STransform t child   -> STransform t <$> parseShapeTree child
  SOverlap overlap above below ->
      do  above' <- parseShapeTree above
          below' <- parseShapeTree below
          return $ SOverlap overlap above' below'

class Combinable o t where
  combine :: o -> t -> t -> t

instance Combinable () [(ShapeHeader, Group (Shaper (Group (Outline DisplaySpace))))] where
  combine () over under = over ++ under

instance Combinable CombineType (Group (Combiner (Group (Outline DisplaySpace)))) where
  combine combineType (Group above) (Group below) =
      let inverter =
              case combineType of
                  CombineSubtract -> map (over coCombineType invertCombineType)
                  _ -> id
      in  Group $ above ++ inverter below

transformShapeTree :: forall t s o leaf . (Transformable t s)
                   => ((t s -> t s) -> leaf -> leaf)
                   -> STree o (TransformType s) leaf
                   -> STree o (TransformType s) leaf
transformShapeTree f tree =
  case tree of
      SLeaf rep -> SLeaf rep
      STransform t child ->
          fmap (f (applyTransformType t)) $ transformShapeTree f child
      SOverlap overlap above below ->
          let above' = transformShapeTree f above
              below' = transformShapeTree f below
          in  SOverlap overlap above' below'

combineShapeTree :: Combinable o a => STree o trans a -> a
combineShapeTree = \case
  SLeaf rep -> rep
  SOverlap overlap above below ->
     let above' = combineShapeTree above
         below' = combineShapeTree below
     in  combine overlap above' below'
  STransform t child -> error "shapeTransforms should be removed before combining"

mkPrimitive :: GroupId -> Substance a -> Combiner rep -> Shaper rep
mkPrimitive token substance (Combiner combineType rep) = Shaper (ShapeInfo (substanceToSubstanceType substance) combineType token) rep

buildPrimitives :: SRep GroupId PictId (Group (Combiner t)) -> (ShapeHeader, Group (Shaper t))
buildPrimitives (SRep token substance rep) = (ShapeHeader substance, fmap (mkPrimitive token substance) rep)

defaultCombineType :: t -> Group (Combiner t)
defaultCombineType a = Group [Combiner CombineAdd a]

traverseShapeTree :: (Monad m)
                 => [PictureMemory]
                 -> Point2 DisplaySpace
                 -> ShapeTreeRoot
                 -> EnclosureMonad m ( [ShapeHeader]
                                     , [[Shaper (BoundingBox, Enclosure)]]
                                     , ShapeTreeState Int)
traverseShapeTree pictureMems canvasSize (ShapeRoot backgroundColor shapeTree) =
    do  (curveTable, sectionSize) <- get
        let (parsedShapeTree, shapeTreeState) = evalShapeTreeMonad pictureMems (parseShapeTree shapeTree)
            outlineTree :: STree () (TransformType DisplaySpace) (SRep GroupId PictId (STree CombineType (TransformType DisplaySpace) Outlines))
            outlineTree = fmap (over shapeCompoundTree (fmap (Group . rawShapeToOutlines))) parsedShapeTree
            transformedTree :: STree () (TransformType DisplaySpace) (SRep GroupId PictId (STree CombineType (TransformType DisplaySpace) Outlines))
            transformedTree = transformShapeTree (fmap . fmap . fmap) . (fmap . fmap $ transformShapeTree fmap) $ outlineTree
            combinedCompounds :: STree () (TransformType DisplaySpace) (SRep GroupId PictId (Combiners Outlines))
            combinedCompounds = fmap (over shapeCompoundTree (combineShapeTree . fmap defaultCombineType)) transformedTree
            curveShapes :: [(ShapeHeader, Shapers Outlines)]
            curveShapes = combineShapeTree . fmap (pure . buildPrimitives) $ combinedCompounds
            boundedShapedEnclosures = parMap rpar {- map -} (makeCurveEnclosures curveTable sectionSize canvasSize) $ map snd curveShapes
            shapes = map fst curveShapes
        return (shapes, boundedShapedEnclosures, shapeTreeState)

-- * Instances

instance NFData token => NFData (ShapeTreeState token) where
  rnf (ShapeTreeState i tokenMap pictRef refs mems) = i        `deepseq`
                                                      tokenMap `deepseq`
                                                      pictRef  `deepseq`
                                                      refs     `deepseq`
                                                      mems     `deepseq` ()
