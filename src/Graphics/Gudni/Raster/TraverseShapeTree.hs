{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Graphics.Gudni.Raster.TraverseShapeTree
  ( EnclosureMonad (..)
  , PrimEntry(..)
  , runEnclosureMonad
  , PrimId (..)
  , PrimEnclosure
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
import Graphics.Gudni.Raster.Primitive

import Graphics.Gudni.Util.Bag
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

type PrimId = Reference PrimEnclosure
type PrimEnclosure = (Primitive, Enclosure)

data PrimEntry = PrimEntry
    { primId :: PrimId
    , primStrandCount :: NumStrands
    , primBox :: BoundingBox
    } deriving (Show)

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
                    -> [(Primitive, [Outline DisplaySpace])]
                    -> [(BoundingBox, PrimEnclosure)]
makeCurveEnclosures curveTable sectionSize canvasSize  =
    catMaybes . map (makeCurveEnclosure curveTable sectionSize canvasSize)

makeCurveEnclosure :: CurveTable
                   -> Int
                   -> Point2 DisplaySpace
                   -> (Primitive, [Outline DisplaySpace])
                   -> Maybe (BoundingBox, PrimEnclosure)
makeCurveEnclosure curveTable sectionSize canvasSize (prim, curves) =
     let boundingBox = getBoundingBox (map getBoundingBox curves)
     in  if excludeBox canvasSize boundingBox
         then Nothing
         else let enclosure = enclose curveTable sectionSize curves
              in  Just (boundingBox, (prim, enclosure))

instance NFData token => NFData (ShapeTreeState token) where
  rnf (ShapeTreeState i tokenMap pictRef refs mems) = i        `deepseq`
                                                      tokenMap `deepseq`
                                                      pictRef  `deepseq`
                                                      refs     `deepseq`
                                                      mems     `deepseq` ()

type ShapeTreeMonad token = State (ShapeTreeState token)

evalShapeTreeMonad :: [PictureMemory]
                   -> ShapeTreeMonad token a
                   -> (a, ShapeTreeState token)
evalShapeTreeMonad pictureMems mf = runState mf (ShapeTreeState (GroupId 0) M.empty 0 [] pictureMems)

parseShapeTree :: (Ord token)
               =>                       STree overlap (SRep token (PictureRef PictId) a)
               -> ShapeTreeMonad token (STree overlap (SRep GroupId PictId            a))
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

instance Combinable () [(ShapeHeader, [(Primitive, [Outline DisplaySpace])])] where
  combine () over under = over ++ under

instance Combinable CombineType [(CombineType, [Outline DisplaySpace])] where
  combine combineType over under =
      let inverter =
              case combineType of
                  CombineSubtract -> map (fmapFst invertCombineType)
                  _ -> id
      in  over ++ inverter under

transformShapeTree :: (Transformable a)
                   => STree o a -> STree o a
transformShapeTree = \case
  SLeaf rep -> SLeaf rep
  STransform t child -> fmap (applyTransformType t) $ transformShapeTree child
  SOverlap overlap above below ->
     let above' = transformShapeTree above
         below' = transformShapeTree below
     in  SOverlap overlap above' below'

combineShapeTree :: Combinable o a => STree o a -> a
combineShapeTree = \case
  SLeaf rep -> rep
  SOverlap overlap above below ->
     let above' = combineShapeTree above
         below' = combineShapeTree below
     in  combine overlap above' below'
  STransform t child -> error "shapeTransforms should be removed before combining"

checkContinue :: Bool -> CombineType
checkContinue isContinue = if isContinue then CombineContinue else CombineAdd

fmapFst f (x,y) = (f x, y)
fmapSnd f (x,y) = (x, f y)

bagSnd :: Bag PrimId PrimEnclosure -> (BoundingBox, PrimEnclosure) -> (Bag PrimId PrimEnclosure, PrimEntry)
bagSnd bag (box, primEnclosure) = let (bag', newPrimId) = addToBag bag primEnclosure
                    in  (bag', PrimEntry newPrimId (enclosureNumStrands $ snd primEnclosure) box)

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
bagShapes :: [(BoundingBox, PrimEnclosure)]
          -> (Bag PrimId PrimEnclosure, [PrimEntry])
bagShapes boxPrimEnclosures = mapAccumL bagSnd emptyBag boxPrimEnclosures

mkPrimitive :: GroupId -> Substance s -> (CombineType, rep) -> (Primitive, rep)
mkPrimitive token substance (combineType, rep) = (Primitive (substanceToSubstanceType substance) combineType token, rep)

buildPrimitives :: SRep GroupId PictId [(CombineType, a)] -> (ShapeHeader, [(Primitive, a)])
buildPrimitives (SRep token substance rep) = (ShapeHeader substance, map (mkPrimitive token substance) rep)

defaultCombineType a = [(CombineAdd, a)]

instance SimpleTransformable (CombineType, [Outline DisplaySpace]) where
    tTranslate p = fmapSnd (tTranslate p)
    tScale     s = fmapSnd (tScale     s)
instance Transformable (CombineType, [Outline DisplaySpace]) where
    tRotate    r = fmapSnd (tRotate    r)

traverseShapeTree :: (Monad m)
                 => [PictureMemory]
                 -> Point2 DisplaySpace
                 -> ShapeTreeRoot
                 -> EnclosureMonad m ( [ShapeHeader]
                                     , Bag PrimId PrimEnclosure
                                     , [PrimEntry]
                                     , ShapeTreeState Int)
traverseShapeTree pictureMems canvasSize (ShapeRoot backgroundColor shapeTree) =
    do  (curveTable, sectionSize) <- get
        let (parsedShapeTree, shapeTreeState) = evalShapeTreeMonad pictureMems (parseShapeTree shapeTree)
            outlineTree = fmap (over shapeCompoundTree (fmap rawShapeToOutlines)) parsedShapeTree
            combinedCompounds = fmap (over shapeCompoundTree (combineShapeTree . fmap defaultCombineType . transformShapeTree)) outlineTree
            curveShapes :: [(ShapeHeader, [(Primitive, [Outline DisplaySpace])])]
            curveShapes = combineShapeTree . fmap (pure . buildPrimitives) . transformShapeTree $ combinedCompounds
            boxPrimEnclosures = parMap rpar {- map -} (makeCurveEnclosures curveTable sectionSize canvasSize) $ map snd curveShapes
            (primBag, primEntries) = bagShapes $ concat boxPrimEnclosures
            shapes = map fst curveShapes
        return (shapes, primBag, primEntries, shapeTreeState)
