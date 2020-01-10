{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Projection
  ( CanProject(..)
  , BezierSpace(..)
  , makeBezierSpace
  , bezierSpaceLengths
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug

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

-- | In most cases, it is sufficient to define
-- @projectionWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class (SpaceOf u ~ SpaceOf t, Space (SpaceOf t)) => CanProject u t where
    projection :: u -> t -> t
    default projection :: u -> t -> t
    projection = projectionWithAccuracy 1e-3

    projectionWithAccuracy :: SpaceOf t -> u -> t -> t
    default projectionWithAccuracy :: SpaceOf t -> u -> t -> t
    projectionWithAccuracy accuracy =
        projectionWithStepsAccuracy (maxStepsFromAccuracy accuracy) (Just accuracy)

    projectionWithSteps :: Int -> u -> t -> t
    projectionWithSteps max_steps = projectionWithStepsAccuracy max_steps Nothing

    projectionWithStepsAccuracy :: Int -> Maybe (SpaceOf t) -> u -> t -> t

instance (s ~ (SpaceOf (f (Bezier s))), Space s, Show (f (Bezier s)), Chain f) => CanProject (BezierSpace s) (f (Bezier s)) where
    projectionWithStepsAccuracy max_steps m_accuracy bSpace beziers =
      let fixed :: f (Bezier s)
          fixed = join . fmap deKnob $ beziers
      in  join . fmap (traverseBezierSpace max_steps m_accuracy
                       bSpace) $ fixed

data BezierSpace s = BezierSpace
  { bsStart  :: Point2 s
  , bsStartNormal :: Diff Point2 s
  , bsEnd    :: Point2 s
  , bsEndNormal :: Diff Point2 s
  , bsTree   :: BezierTree s
  , bsLength :: X s
  } deriving (Show)

instance Space s => HasSpace (BezierSpace s) where
  type SpaceOf (BezierSpace s ) = s

data BezierTree s
  = BezierSplit
         { bzTreeSplitX :: X s
         , bzTreeSplitPoint :: Point2 s
         , bzTreeLeftNormal  :: Diff Point2 s
         , bzTreeRightNormal :: Diff Point2 s
         , bzTreeLeft  :: BezierTree s
         , bzTreeRight :: BezierTree s
         }
  | BezierLeaf
         { bzTreeLength :: X s
         , bzTreeControlPoint :: Point2 s
         }
  deriving (Show)

subdivideAcuteBezier :: (Space s, Alternative f) => Bezier s -> f (Bezier s)
subdivideAcuteBezier bz@(Bez v0 vC v1) =
  let tan0 = v0 .-. vC
      tan1 = v1 .-. vC
  in  if angleBetween tan0 tan1 <= 135 @@ deg -- this can probably be optimized with something simpler.
      then let (left, right) = splitClosestControl bz
           in  subdivideAcuteBezier left <|> subdivideAcuteBezier right
      else pure bz

makeBezierSpace :: forall f s . (Eq1 f, Chain f, Space s, Show (f (Bezier s))) => (Bezier s -> X s) -> f (Bezier s) -> BezierSpace s
makeBezierSpace lengthFun chain =
  fromJust . go 0 $ fixedChain
  where
  fixedChain = join . fmap (subdivideAcuteBezier) $ chain
  go :: X s -> f (Bezier s) -> Maybe (BezierSpace s)
  go start vector =
    let (left, right) = halfSplit vector
    in if right `eq1` empty
       then if left `eq1` empty
            then Nothing
            else let (Bez v0 vC v1) = firstLink left
                     curveLength :: X s
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

traverseBezierSpace :: forall s f
                    .  (Space s, Alternative f, Chain f)
                    => Int
                    -> Maybe s
                    -> BezierSpace s
                    -> Bezier s
                    -> f (Bezier s)
traverseBezierSpace max_steps m_accuracy bSpace@(BezierSpace sPoint sNormal ePoint eNormal tree len) item =
  if bezierIsForward item
  then go 0 sPoint sNormal len ePoint eNormal tree item
  else reverseChain . fmap (reverseBezier) . go 0 sPoint sNormal len ePoint eNormal tree . reverseBezier $ item
  where
  go :: X s -> Point2 s -> Diff Point2 s -> X s -> Point2 s -> Diff Point2 s -> BezierTree s -> Bezier s -> f (Bezier s)
  go start sPoint sNormal end ePoint eNormal tree bz =
     let box = boxOf bz
     in
     case tree of
       BezierSplit splitX splitPoint leftNormal rightNormal leftTree rightTree ->
           if box ^. leftSide < splitX
           then
               if box ^. rightSide > splitX
               then let (leftBz, rightBz) = splitBezierX splitX bz
                        leftResult  = go start  sPoint     sNormal     splitX splitPoint leftNormal leftTree  leftBz
                        rightResult = go splitX splitPoint rightNormal end    ePoint     eNormal    rightTree rightBz
                    in  leftResult <|> rightResult
               else go start  sPoint     sNormal     splitX splitPoint leftNormal leftTree  bz
           else      go splitX splitPoint rightNormal end    ePoint     eNormal   rightTree bz
       BezierLeaf curveLength control ->
         if box ^. leftSide < start
         then
              if box ^. rightSide > start
              then let (leftBz, rightBz) = splitBezierX start bz
                       leftResult  = pure (projectTangentBezier start sPoint sNormal leftBz)
                       rightResult = go start sPoint sNormal end ePoint eNormal tree rightBz
                   in  leftResult <|> rightResult
              else pure (projectTangentBezier start sPoint sNormal bz)
         else if box ^. rightSide > end
              then
                  if box ^. leftSide < end
                  then let (leftBz, rightBz)  = splitBezierX end bz
                           leftResult  = go start sPoint sNormal end ePoint eNormal tree leftBz
                           rightResult = pure (projectTangentBezier end ePoint eNormal rightBz)
                       in  leftResult <|> rightResult
                  else pure (projectTangentBezier end ePoint eNormal bz)
              else pure (mkOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control curveLength bz)

projectTangentPoint :: Space s => X s -> Point2 s -> Diff Point2 s -> Point2 s -> Point2 s
projectTangentPoint offset v0 normal (Point2 x y) =
  let t = x - unOrtho offset
      tangent = negate $ perp normal
  in  v0 .+^ (t *^ tangent) .+^ (y *^ normal)

projectTangentBezier :: Space s => X s -> Point2 s -> Diff Point2 s -> Bezier s -> Bezier s
projectTangentBezier offset v0 normal bz = overBezier (projectTangentPoint offset v0 normal) bz

projectOffsetCurve :: Space s
                   => Int
                   -> Maybe s
                   -> X s
                   -> Point2 s
                   -> Diff Point2 s
                   -> X s
                   -> Point2 s
                   -> Diff Point2 s
                   -> Point2 s
                   -> X s
                   -> Bezier s
                   -> Bezier s
projectOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
    let sourceCurve = Bez sPoint control ePoint
        correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - unOrtho start) -- /unOrtho len
        (V3 t0 tc t1) = fmap (correctX . unOrtho . view pX) . view bzPoints $ bz
        (V3 y0 yC y1) = fmap (unOrtho . view pY) . view bzPoints $ bz
        (normal0, normal1, s0, sC, s1) =
            if t0 == t1
            then let (Bez s0 sC s1) = dropBezier t0 sourceCurve
                     n0 = normalize (perp (sC .-. s0))
                 in  (n0, n0, s0, s0, s0)
            else let (Bez s0 sC s1) = sliceBezier t0 t1 sourceCurve
                     n0 = normalize (perp (sC .-. s0))
                     n1 = normalize (perp (s1 .-. sC))
                 in  (n0, n1, s0, sC, s1)
        m = normal0 ^+^ normal1
        sOffset = normal0 ^* y0
        cOffset = m ^* (2 * yC / m `dot` m) -- probably wrong
        eOffset = normal1 ^* y1
        s = s0 .+^ sOffset
        c = sC .+^ cOffset
        e = s1 .+^ eOffset
    in  Bez s c e

bezierIsForward (Bez v0 _ v1) = v0 ^. pX <= v1 ^. pX

mkOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
  let prj = projectOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len
  in
  if bezierIsForward bz
  then prj bz
  else reverseBezier .
       prj .
       reverseBezier $
       bz

bezierSpaceLengths :: Alternative t => BezierSpace s -> t s
bezierSpaceLengths = go . bsTree
 where
 go node =
   case node of
    BezierSplit {} -> go (bzTreeLeft node) <|> go (bzTreeRight node)
    BezierLeaf {} -> pure . unOrtho . bzTreeLength $ node
