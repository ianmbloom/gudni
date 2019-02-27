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
  , TileGrid (..)
  , tGTileSize
  , tGScreenSize
  , tGGridSize
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
  do let curveTable = buildCurveTable sECTIONsIZE
     (evalStateT code) (curveTable, sECTIONsIZE)

type PrimId = Reference PrimEnclosure
type PrimEnclosure = (Primitive, Enclosure)

data TileGrid = TileGrid
    { _tGScreenSize :: !(Point2 DisplaySpace)
    , _tGTileSize   :: !(Point2 DisplaySpace)
    , _tGGridSize   :: !(Point2 IntSpace)
    } deriving (Show)
makeLenses ''TileGrid

constrainBox :: TileGrid
             -> Box DisplaySpace
             -> Maybe (Box DisplaySpace)
constrainBox (TileGrid displaySize _ _) boundingBox =
               let  left            = min (boundingBox ^. leftSide  ) (displaySize ^. pX)
                    right           = max (boundingBox ^. rightSide )  0
                    top             = min (boundingBox ^. topSide    ) (displaySize ^. pY)
                    bottom          = max (boundingBox ^. bottomSide)  0
               in   if right <= 0 || bottom <= 0 || left >= displaySize ^. pX || top >= displaySize ^. pY
                    then Nothing
                    else Just $ makeBox (makePoint left top) (makePoint right bottom)

blockBox :: TileGrid
         -> Box DisplaySpace
         -> Box IntSpace
blockBox (TileGrid _ tileSize gridSize) boundingBox =
    let  gridSizeX       = (fromIntegral $ gridSize ^. pX) - 0.5
         gridSizeY       = (fromIntegral $ gridSize ^. pY) - 0.5
         convertClampX x = Ortho . ISpace . fromIntegral . fastTruncateSingle . clamp 0.5 gridSizeX . realToFrac $ x / tileSize ^. pX
         convertClampY y = Ortho . ISpace . fromIntegral . fastTruncateSingle . clamp 0.5 gridSizeY . realToFrac $ y / tileSize ^. pY
         leftG           = convertClampX (boundingBox ^. leftSide  )
         rightG          = convertClampX (boundingBox ^. rightSide )
         topG            = convertClampY (boundingBox ^. topSide   )
         bottomG         = convertClampY (boundingBox ^. bottomSide)
    in   makeBox (makePoint leftG topG) (makePoint rightG bottomG)

makeCurveEnclosures :: CurveTable
                    -> Int
                    -> TileGrid
                    -> [(Primitive, [Outline DisplaySpace])]
                    -> [(Block, PrimEnclosure)]
makeCurveEnclosures curveTable sectionSize tileGrid prims = catMaybes . map (makeCurveEnclosure curveTable sectionSize tileGrid) $ prims

makeCurveEnclosure :: CurveTable
                   -> Int
                   -> TileGrid
                   -> (Primitive, [Outline DisplaySpace])
                   -> Maybe (Block, PrimEnclosure)
makeCurveEnclosure curveTable sectionSize tileGrid (prim, curves) =
     let boundingBox = boxBoxes (map outlineBox curves)
         mConstrainedBox = constrainBox tileGrid boundingBox
     in  case mConstrainedBox of
            Just constrainedBox -> let block = blockBox tileGrid constrainedBox
                                       enclosure = enclose curveTable sectionSize curves
                                   in  Just (block, (prim, enclosure))
            Nothing             -> Nothing

data ShapeTreeState token = ShapeTreeState
  { _stGroupId           :: GroupId
  , _stTokenMap          :: M.Map token GroupId
  , _stCurrentPictureRef :: Int
  , _stPictureRefs       :: [PictureRef PictureMemory]
  , _stPictureMems       :: [PictureMemory]
  }
makeLenses ''ShapeTreeState

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

bagSnd :: Integral a => Bag a b -> (c, b) -> (Bag a b, (c, a))
bagSnd bag (c, a) = let (bag', b) = addToBag bag a
                    in  (bag', (c, b))

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
bagShapes :: [(Block, PrimEnclosure)]
          -> (Bag PrimId PrimEnclosure, [(Block, PrimId)])
bagShapes blockPrimEnclosures = mapAccumL bagSnd emptyBag blockPrimEnclosures

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
                 -> TileGrid
                 -> ShapeTreeRoot
                 -> EnclosureMonad m ( [ShapeHeader]
                                     , Bag PrimId PrimEnclosure
                                     , [(Block, PrimId)]
                                     , ShapeTreeState Int)
traverseShapeTree pictureMems tileGrid (ShapeRoot backgroundColor shapeTree) =
    do  (curveTable, sectionSize) <- get
        let (parsedShapeTree, shapeTreeState) = evalShapeTreeMonad pictureMems (parseShapeTree shapeTree)
            outlineTree = fmap (over shapeCompoundTree (fmap rawShapeToCurve)) parsedShapeTree
            combinedCompounds = fmap (over shapeCompoundTree (combineShapeTree . fmap defaultCombineType . transformShapeTree)) outlineTree
            curveShapes :: [(ShapeHeader, [(Primitive, [Outline DisplaySpace])])]
            curveShapes = combineShapeTree . fmap (pure . buildPrimitives) . transformShapeTree $ combinedCompounds
            blockPrimEnclosures = parMap rpar {- map -} (makeCurveEnclosures curveTable sectionSize tileGrid) $ map snd curveShapes
            (primBag, primBlocks) = bagShapes $ concat blockPrimEnclosures
            shapes = map fst curveShapes
        return (shapes, primBag, primBlocks, shapeTreeState)
