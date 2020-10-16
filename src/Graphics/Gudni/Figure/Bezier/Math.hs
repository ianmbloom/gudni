{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Bezier.Math
   ( projectTangentPoint
   , projectTangentBezier
   , bezierPointAndNormal
   , bezierStartTangent
   , bezierStartNormal
   , bezierEndTangent
   , bezierEndNormal
   , relativeToNormalVector
   , slopeOf
   , yInterceptSlope
   , xInterceptSlope
   , arbitraryIntersection
   , projPoint
   , offsetBezier
   , midOffsetBezier
   )
where

import Graphics.Gudni.Figure.Principle

import Linear.Vector
import Linear.Affine
import Linear.Metric
import Linear.V2

import Control.Lens

projectTangentPoint :: Space s => Ax Horizontal s -> Point2 s -> Diff Point2 s -> Point2 s -> Point2 s
projectTangentPoint offset v0 normal p =
  let t = p ^. pX - offset
      tangent = negate $ perp normal
  in  v0 .+^ (fromAlong Horizontal t *^ tangent) .+^ (fromAlong Vertical (p ^. pY) *^ normal)

projectTangentBezier :: Space s => Ax Horizontal s -> Point2 s -> Diff Point2 s -> Bezier s -> Bezier s
projectTangentBezier offset v0 normal bz = overBezier (projectTangentPoint offset v0 normal) bz

bezierPointAndNormal :: Space s => Bezier s -> s -> (Point2 s, Diff V2 s)
bezierPointAndNormal sourceCurve t =
  if t < 0.5
  then let (Bez s0 sC s1) = dropBezier t sourceCurve
           tangent = bezierStartTangent (Bez s0 sC s1)
           n0 = perp tangent
       in  (s0, n0)
  else let (Bez s0 sC s1) = takeBezier t sourceCurve
           tangent = bezierEndTangent (Bez s0 sC s1)
           n0 = perp tangent
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

yInterceptSlope :: Space s => Point2 s -> s -> Ax Horizontal s -> Ax Vertical s
yInterceptSlope v slope x = toAlong Vertical $ slope * (fromAlong Horizontal $ x - v ^. pX) + (fromAlong Vertical $ v ^. pY)

xInterceptSlope :: Space s => Point2 s -> s -> Ax Vertical s -> Ax Horizontal s
xInterceptSlope v slope y = toAlong Horizontal $ (fromAlong Vertical (y - v^.pY) / slope) + fromAlong Horizontal (v ^. pX)

arbitraryIntersection :: Space s => Point2 s -> s -> Point2 s -> s -> Point2 s
arbitraryIntersection p0 slope0 p1 slope1 =
  let x = toAlong Horizontal $ ( slope1 * (fromAlong Horizontal $ p1^.pX) - slope0 * (fromAlong Horizontal $ p0^.pX) - (fromAlong Vertical $ p1 ^. pY) + (fromAlong Vertical $ p0^.pY) ) / ( slope1 - slope0 )
      y = yInterceptSlope p0 slope0 x
  in  makePoint x y

projPoint :: forall s . Space s => Bezier s -> Point2 s -> Point2 s
projPoint curve toProject =
    let (point, normal) = bezierPointAndNormal curve (fromAlong Horizontal $ toProject ^. pX)
    in  point .+^ (((fromAlong Vertical $ toProject ^. pY) *^ normal) :: Diff V2 s)

controlOffset :: Space s => s -> V2 s -> V2 s -> V2 s
controlOffset d m1 m2 =
    let m = m1 ^+^ m2
    in  m ^* ((2 * d) / quadrance m)


offsetBezier :: Space s => s -> Bezier s -> Bezier s
offsetBezier d bez =
  let m1 = bezierStartNormal bez
      m2 = bezierEndNormal bez
      k  = controlOffset d m1 m2
      s  = bez ^. bzStart   ^+^ (P (m1 ^* d) )
      c  = bez ^. bzControl ^+^ (P k         )
      e  = bez ^. bzEnd     ^+^ (P (m2 ^* d) )
  in  Bez s c e

midOffsetBezier :: Space s => s -> Bezier s -> Bezier s
midOffsetBezier d bez =
    let m1 = bezierStartNormal bez
        m2 = bezierEndNormal bez
        k  = controlOffset (d/2) m1 m2
        s  = bez ^. bzStart   -- ^+^ (P (m1 ^* d) )
        c  = bez ^. bzControl ^+^ (P k         )
        e  = bez ^. bzEnd     ^+^ (P (m2 ^* d) )
    in  Bez s c e
