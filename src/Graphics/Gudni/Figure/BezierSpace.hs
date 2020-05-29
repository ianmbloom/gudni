{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.BezierSpace
  ( BezierSpace(..)
  , makeBezierSpace
  , traverseBezierSpace
  , bezierSpaceLengths
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Maybe (fromMaybe, fromJust)

data BezierSpace s = BezierSpace
  { bsStart       :: Point2 s
  , bsStartNormal :: Diff Point2 s
  , bsEnd         :: Point2 s
  , bsEndNormal   :: Diff Point2 s
  , bsTree        :: BezierTree s
  , bsLength      :: s
  } deriving (Show)

instance Space s => HasSpace (BezierSpace s) where
  type SpaceOf (BezierSpace s ) = s

data BezierTree s
  = BezierSplit
         { bzTreeSplitX :: s
         , bzTreeSplitPoint  :: Point2 s
         , bzTreeLeftNormal  :: Diff Point2 s
         , bzTreeRightNormal :: Diff Point2 s
         , bzTreeLeft  :: BezierTree s
         , bzTreeRight :: BezierTree s
         }
  | BezierLeaf
         { bzTreeLength :: s
         , bzTreeControlPoint :: Point2 s
         }
  deriving (Show)

subdivideAcuteBezier :: (Space s, Alternative f) => Int -> Bezier s -> f (Bezier s)
subdivideAcuteBezier iterations bz@(Bez v0 vC v1) =
  let tan0 = v0 .-. vC
      tan1 = v1 .-. vC
  in  if iterations > 0 && angleBetween tan0 tan1 <= 135 @@ deg -- this can probably be optimized with something simpler.
      then let (left, right) = splitClosestControl bz
           in  subdivideAcuteBezier (iterations - 1) left <|> subdivideAcuteBezier (iterations - 1) right
      else pure bz

makeBezierSpace :: forall f s . (Eq1 f, Chain f, Space s) => (Bezier s -> s) -> f (Bezier s) -> BezierSpace s
makeBezierSpace lengthFun chain =
  fromJust . go 0 $ fixedChain
  where
  fixedChain = join . fmap (subdivideAcuteBezier 3) $ chain
  go :: s -> f (Bezier s) -> Maybe (BezierSpace s)
  go start vector =
    let (left, right) = halfSplit vector
    in if right `eq1` empty
       then if left `eq1` empty
            then Nothing
            else let (Bez v0 vC v1) = firstLink left
                     curveLength :: s
                     curveLength = lengthFun (Bez v0 vC v1)
                     normal0 = normalize (perp (vC .-. v0))
                     normal1 = normalize (perp (v1 .-. vC))
                     node :: BezierTree s
                     node = BezierLeaf curveLength vC
                 in  Just $ BezierSpace v0 normal0 v1 normal1 node (start + curveLength)
        else
           let makeSplit (BezierSpace lLPoint  lLNormal lRPoint lRNormal leftTree  leftOffset )
                         (BezierSpace _rLPoint rLNormal rRPoint rRNormal rightTree rightOffset) =
                         let node = BezierSplit
                                      { bzTreeSplitX = leftOffset
                                      , bzTreeSplitPoint = lRPoint
                                      , bzTreeLeftNormal = lRNormal
                                      , bzTreeRightNormal = rLNormal
                                      , bzTreeLeft  = leftTree
                                      , bzTreeRight = rightTree
                                      }
                         in BezierSpace lLPoint lLNormal rRPoint rRNormal node rightOffset
           in  case go start left of
                   Just leftSpace ->
                       case go (bsLength leftSpace) right of
                           Just rightSpace -> Just $ makeSplit leftSpace rightSpace
                           Nothing -> Just $ leftSpace
                   Nothing -> Nothing


traverseBezierSpace :: forall t f
                    .  ( Alternative f
                       , Chain f
                       , Space (SpaceOf t)
                       , CanCut t
                       , CanFit t
                       , HasBox t
                       )
                    => Bool
                    -> Int
                    -> Maybe (SpaceOf t)
                    -> BezierSpace (SpaceOf t)
                    -> t
                    -> f t
traverseBezierSpace debug max_steps m_accuracy bSpace@(BezierSpace sPoint sNormal ePoint eNormal tree len) item =
  if isForward item
  then go 0 sPoint sNormal len ePoint eNormal tree item
  else reverseChain . fmap (reverseItem) . go 0 sPoint sNormal len ePoint eNormal tree . reverseItem $ item
  where
  go :: SpaceOf t -> Point2 (SpaceOf t) -> Diff Point2 (SpaceOf t) -> SpaceOf t -> Point2 (SpaceOf t) -> Diff Point2 (SpaceOf t) -> BezierTree (SpaceOf t) -> t -> f t
  go start sPoint sNormal end ePoint eNormal tree bz =
     let box = boxOf bz
     in
     case tree of
       BezierSplit splitX splitPoint leftNormal rightNormal leftTree rightTree ->
           if box ^. leftSide < splitX
           then
               if box ^. rightSide > splitX
               then let (leftItem, rightItem) = splitAtCut pX splitX bz
                        leftResult  = go start  sPoint     sNormal     splitX splitPoint leftNormal leftTree  leftItem
                        rightResult = go splitX splitPoint rightNormal end    ePoint     eNormal    rightTree rightItem
                    in  fillGap leftResult rightResult
               else go start  sPoint     sNormal     splitX splitPoint leftNormal leftTree  bz
           else     go splitX splitPoint rightNormal end    ePoint     eNormal   rightTree bz
       BezierLeaf curveLength control ->
           if box ^. leftSide < start
           then if box ^. rightSide > start
                then let (leftItem, rightItem) = splitAtCut pX start bz
                         leftResult  = pure (projectTangent start sPoint sNormal leftItem)
                         rightResult = go start sPoint sNormal end ePoint eNormal tree rightItem
                     in  fillGap leftResult rightResult
                else pure (projectTangent start sPoint sNormal bz)
           else if box ^. rightSide > end
                then if box ^. leftSide < end
                     then let (leftItem, rightItem)  = splitAtCut pX end bz
                              leftResult  = go start sPoint sNormal end ePoint eNormal tree leftItem
                              rightResult = pure (projectTangent end ePoint eNormal rightItem)
                          in  fillGap leftResult rightResult
                     else pure (projectTangent end ePoint eNormal bz)
                else pure . projectOntoCurve debug max_steps m_accuracy start (Bez sPoint control ePoint) $ bz

bezierSpaceLengths :: Alternative t => BezierSpace s -> t s
bezierSpaceLengths = go . bsTree
 where
 go node =
   case node of
    BezierSplit {} -> go (bzTreeLeft node) <|> go (bzTreeRight node)
    BezierLeaf  {} -> pure . bzTreeLength $ node
