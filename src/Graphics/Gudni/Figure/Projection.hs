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

-- | In most cases, it is sufficient to define
-- @projectionWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class (SpaceOf u ~ SpaceOf t, Space (SpaceOf t)) => CanProject u t where
    projectOnto :: Bool -> u -> t -> t
    default projectOnto :: Bool -> u -> t -> t
    projectOnto debug = projectionWithAccuracy debug 1e-3

    projectionWithAccuracy :: Bool ->SpaceOf t -> u -> t -> t
    default projectionWithAccuracy :: Bool -> SpaceOf t -> u -> t -> t
    projectionWithAccuracy debug accuracy =
        projectionWithStepsAccuracy debug (maxStepsFromAccuracy accuracy) (Just accuracy)

    projectionWithSteps :: Bool -> Int -> u -> t -> t
    projectionWithSteps debug max_steps = projectionWithStepsAccuracy debug max_steps Nothing

    projectionWithStepsAccuracy :: Bool -> Int -> Maybe (SpaceOf t) -> u -> t -> t

instance (s ~ (SpaceOf (f (Bezier s))), Space s, Show (f (Bezier s)), Chain f) => CanProject (BezierSpace s) (f (Bezier s)) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace beziers =
      let fixed :: f (Bezier s)
          fixed = join . fmap deKnob $ beziers
      in  trWhen debug  "projected" $
          join . fmap (traverseBezierSpace debug max_steps m_accuracy
                       bSpace) $ fixed

data BezierSpace s = BezierSpace
  { bsStart  :: Point2 s
  , bsStartNormal :: Diff Point2 s
  , bsEnd    :: Point2 s
  , bsEndNormal :: Diff Point2 s
  , bsTree   :: BezierTree s
  , bsLength :: s
  } deriving (Show)

instance Space s => HasSpace (BezierSpace s) where
  type SpaceOf (BezierSpace s ) = s

data BezierTree s
  = BezierSplit
         { bzTreeSplitX :: s
         , bzTreeSplitPoint :: Point2 s
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

subdivideAcuteBezier :: (Space s, Alternative f) => Bezier s -> f (Bezier s)
subdivideAcuteBezier bz@(Bez v0 vC v1) =
  let tan0 = v0 .-. vC
      tan1 = v1 .-. vC
  in  if angleBetween tan0 tan1 <= 135 @@ deg -- this can probably be optimized with something simpler.
      then let (left, right) = splitClosestControl bz
           in  subdivideAcuteBezier left <|> subdivideAcuteBezier right
      else pure bz

makeBezierSpace :: forall f s . (Eq1 f, Chain f, Space s, Show (f (Bezier s))) => (Bezier s -> s) -> f (Bezier s) -> BezierSpace s
makeBezierSpace lengthFun chain =
  fromJust . go 0 $ fixedChain
  where
  fixedChain = join . fmap (subdivideAcuteBezier) $ chain
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

traverseBezierSpace :: forall s f
                    .  (Space s, Alternative f, Chain f)
                    => Bool
                    -> Int
                    -> Maybe s
                    -> BezierSpace s
                    -> Bezier s
                    -> f (Bezier s)
traverseBezierSpace debug max_steps m_accuracy bSpace@(BezierSpace sPoint sNormal ePoint eNormal tree len) item =
  if bezierIsForward item
  then go 0 sPoint sNormal len ePoint eNormal tree item
  else reverseChain . fmap (reverseBezier) . go 0 sPoint sNormal len ePoint eNormal tree . reverseBezier $ item
  where
  go :: s -> Point2 s -> Diff Point2 s -> s -> Point2 s -> Diff Point2 s -> BezierTree s -> Bezier s -> f (Bezier s)
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
              else pure (mkOffsetCurve debug max_steps m_accuracy start sPoint sNormal end ePoint eNormal control curveLength bz)

projectTangentPoint :: Space s => s -> Point2 s -> Diff Point2 s -> Point2 s -> Point2 s
projectTangentPoint offset v0 normal (Point2 x y) =
  let t = x - offset
      tangent = negate $ perp normal
  in  v0 .+^ (t *^ tangent) .+^ (y *^ normal)

projectTangentBezier :: Space s => s -> Point2 s -> Diff Point2 s -> Bezier s -> Bezier s
projectTangentBezier offset v0 normal bz = overBezier (projectTangentPoint offset v0 normal) bz

bezierPointAndNormal sourceCurve t =
  if t < 0.5
  then let (Bez s0 sC s1) = dropBezier t sourceCurve
           tangent0 = sC .-. s0
           n0 = normalize (perp (tangent0))
       in  (s0, n0)
  else let (Bez s0 sC s1) = takeBezier t sourceCurve
           tangent0 = s1 .-. sC
           n0 = normalize (perp (tangent0))
       in  (s1, n0)

bezierStartTangent :: Space s => Bezier s -> Diff V2 s
bezierStartTangent (Bez s0 sC s1) = normalize (sC .-. s0)

bezierStartNormal :: Space s => Bezier s -> Diff V2 s
bezierStartNormal bz = perp (bezierStartTangent bz)

bezierEndTangent :: Space s => Bezier s -> Diff V2 s
bezierEndTangent (Bez s0 sC s1) = normalize (s1 .-. sC)

bezierEndNormal :: Space s => Bezier s -> Diff V2 s
bezierEndNormal bz = perp (bezierEndTangent bz)

relativeToNormalVector :: Space s => Diff V2 s -> Diff V2 s -> Diff V2 s
relativeToNormalVector source@(V2 sX sY) dest@(V2 dX dY) = (negate dX *^ perp source) ^+^ (dY *^ source)

slopeOf :: Space s => Diff V2 s -> s
slopeOf (V2 x y) = y / x

yInterceptSlope :: Space s => Point2 s -> s -> s -> s
yInterceptSlope v slope x = slope * (x - v^.pX) + v^.pY

xInterceptSlope :: Space s => Point2 s -> s -> s -> s
xInterceptSlope v slope y = ((y - v^.pY) / slope) + v^.pX

arbitraryIntersection :: Space s => Point2 s -> s -> Point2 s -> s -> Point2 s
arbitraryIntersection p0 slope0 p1 slope1 =
  let x = ( slope1 * (p1^.pX) - slope0 * (p0^.pX) - (p1^.pY) + (p0^.pY) ) / ( slope1 - slope0 )
      y = yInterceptSlope p0 slope0 x
  in  Point2 x y

projectCurveFromParams debug max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
   projectCurve debug max_steps m_accuracy start (Bez sPoint control ePoint) bz

projectCurve :: forall s
                   .  Space s
                   => Bool
                   -> Int
                   -> Maybe s
                   -> s
                   -> Bezier s
                   -> Bezier s
                   -> Bezier s
-- Given a target curve and a source curve, return a new target curve that approximates projecting every point in the target curve
-- onto the source curve, such that the original x-axis corresponds to arclength along the source curve and the y-axis corresponds
-- to distance from source curve along a normal.
-- We can assume that the target curve has already been split and ordered so that the start x is less than or equal to the conrol x
-- and the control x is less than or equal to the end x. In other words the curve is roughly horizontal.
-- We can also assume that all the x coordinates will be within the range of 0 to the source curves arcLength after correction.
projectCurve debugFlag max_steps m_accuracy start sourceCurve targetCurve =
    let -- Transform an x value into a t-parameter for the source curve
        -- that corresponds to a point x arc-distance along the curve.
        correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - start)
        -- Transform the target curve so that each x value is a t-parameter.
        targetCurveCorrected = over bzPoints (fmap (over pX correctX)) $ targetCurve
        -- Define variables for all of the components of the transformed target curve.
        (V3 t0 tC t1) = fmap (view pX) . view bzPoints $ targetCurveCorrected
        (V3 y0 yC y1) = fmap (view pY) . view bzPoints $ targetCurveCorrected
    in  if t0 == t1 -- the curve is vertical.
        then let (s0, normal0) = bezierPointAndNormal sourceCurve t0
                 -- Just project the start and end points
                 s = s0 .+^ (normal0 ^* y0)
                 e = s0 .+^ (normal0 ^* y1)
                 c = mid s e
             in  Bez s c e
        else let -- Find the original start tangent in the target curve's untransformed space
                 targetTangent0 = bezierStartTangent targetCurve
                 -- Find the original end tangent in the target curve's untransformed space
                 targetTangent1 = bezierEndTangent   targetCurve

                 -- Find the point along the source curve that corresponds to the start of the target curve
                 -- and the normal vector from that point.
                 (start, normal0)  = bezierPointAndNormal sourceCurve t0
                 -- Find the point along the source curve that corresponds to the end of the target curve
                 -- and the normal vector from that point.
                 (end,   normal1)  = bezierPointAndNormal sourceCurve t1

                 -- Find the projected location of the new target curves start point.
                 s0 = start .+^ (normal0 ^* y0)
                 -- Find the projected location of the new target curves end point.
                 s1 = end .+^ (normal1 ^* y1)

                 -- Rotate the original start tangent of target curve using the start normal from the source curve.
                 tangent0Rotated = relativeToNormalVector normal0 targetTangent0
                 -- Rotate the original end tangent of target curve using the end normal from the source curve.
                 tangent1Rotated = relativeToNormalVector normal1 targetTangent1
                 -- Define the slopes of the tangent lines. (Because haskell is call by need these won't actually be calculated until
                 -- we know the lines aren't verticle.
                 slope0 = slopeOf tangent0Rotated
                 slope1 = slopeOf tangent1Rotated
                 -- calculate the new control point by finding the intersection between the rotated tangent lines.
                 c
                   | tangent0Rotated ^. _x == 0 && tangent1Rotated ^. _x == 0 = mid s0 s1 -- both tangents are vertical so just use the midpoint.
                   | tangent0Rotated ^. _x == 0 = Point2 (s0^.pX) (yInterceptSlope s1 slope1 (s0 ^. pX)) -- one tangent line is vertical so use yIntercept.
                   | tangent1Rotated ^. _x == 0 = Point2 (s1^.pX) (yInterceptSlope s0 slope0 (s1 ^. pX))
                   | abs (slope0 - slope1) < 0.001 = mid s0 s1 -- lines are too close to parallel so use the midpoint.
                     -- otherwise calculate the intersection of the two tangent lines.
                     -- (temporarily clamping y below so that misbehaving intersection points don't crash the rest of the rasterizer.)
                   | otherwise = over pY (clamp (-10000) 10000) $ arbitraryIntersection s0 slope0 s1 slope1
             in -- return the resulting curve.
                Bez s0 c s1

bezierIsForward (Bez v0 _ v1) = v0 ^. pX <= v1 ^. pX

mkOffsetCurve debug max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
  let prj = projectCurveFromParams debug max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len
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
    BezierLeaf  {} -> pure . bzTreeLength $ node
