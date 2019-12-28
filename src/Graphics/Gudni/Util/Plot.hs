{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.Plot
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for turning the course of a turtle into an openCurve.

module Graphics.Gudni.Util.Plot
  ( curveLibrary
  , allTurtles
  , turtleNames
  , plotTurtle
  , roundedPolygon
  , makeArc
  , star
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformable

import Graphics.Gudni.Util.Debug

import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens

makeArcSegment :: (Alternative t, Space s) => Angle s -> OpenCurve_ t s
makeArcSegment angle = OpenCurve . pure $ curved (Point2 1 0) (Point2 1 (tanA $ angle ^/ 2)) (Point2 (cosA angle) (sinA angle))

makeArc :: (UnLoop t, Alternative t, Space s) => Angle s -> OpenCurve_ t s
makeArc angle
    | abs (angle ^. deg) < 45 = makeArcSegment angle
    | abs (angle ^. deg) == 0 = OpenCurve empty
    | otherwise = makeArc (angle ^/ 2) <^> overCurvePoints (rotateBy (angle ^/ 2)) (makeArc (angle ^/ 2))

data Turn = Hard | Smooth deriving (Ord, Eq, Show)
data LR s = L | R | UTurn | Arb (Angle s) deriving (Ord, Eq, Show)

-- | Simple data structure for creating shapes by moving a turtle.
data Turtle s where
  TGo :: s -> Turtle s
  TRadius :: s -> Turtle s
  TTurn :: Turn -> LR s -> Turtle s
  TReverse :: [Turtle s] -> Turtle s
  TMirror :: [Turtle s] -> Turtle s
  deriving (Ord, Eq, Show)

-- | State constructor for parsing a list of turtle moves.
data TurtleState s = TurtleState
  { _turtleRadius :: s
  , _turtleAngle :: Angle s
  }
makeLenses ''TurtleState

-- | Create an intial state for a turtle.
initialTurtleState :: (Space s) => TurtleState s
initialTurtleState = TurtleState 1 (0 @@ deg)

-- | Add two angles and normalize the result.
(+<+) :: (Space s) => Angle s -> Angle s -> Angle s
(+<+) a b = normalizeAngle (a ^+^ b)

-- | Convert a list of turtle moves into an OpenCurve.
plotTurtle :: (UnLoop t, Alternative t, Space s) => TurtleState s -> [Turtle s] -> OpenCurve_ t s
plotTurtle state ss = let (m_plots, s) = runState (mapM plotTurtle' ss) initialTurtleState
                          plots = catMaybes m_plots
                      in  foldl1 (<^>) plots

-- | Follow turtle moves across a monad.
plotTurtle' :: (UnLoop t, Alternative t, Space s) => Turtle s -> State (TurtleState s) (Maybe (OpenCurve_ t s))
plotTurtle' simp =
  do
    a  <- use turtleAngle
    m_plot <- plotTurtle'' simp
    case m_plot of
      Nothing -> return Nothing
      Just plot -> return $ Just $ overCurvePoints (rotateBy a) plot


plotTurtle'' :: (UnLoop t, Alternative t, Space s) => Turtle s -> State (TurtleState s) (Maybe (OpenCurve_ t s))
plotTurtle'' (TGo distance) = do r <- use turtleRadius ; return $ Just $ OpenCurve $ pure $ straight (Point2 0 0) (Point2 distance 0)
plotTurtle'' (TRadius r) = do turtleRadius .= r; return Nothing
plotTurtle'' (TTurn t lr) =
  do
    let a = lrToAngle lr
    turtleAngle %= (a +<+)
    case t of
      Hard ->
        return Nothing
      Smooth ->
        do
          r <- use turtleRadius
          let f = if a > (0 @@ deg) then overCurvePoints (rotateBy (-90 @@ deg)) else overCurvePoints (rotateBy (90 @@ deg))
          return $ Just $ f $ let arc = overCurvePoints (scaleBy r) $ makeArc a
                                  start = (arc ^. outset)
                              in  overCurvePoints (^-^ start) arc
plotTurtle'' (TReverse pl) =
  do state <- get
     return $ Just $ reverseCurve $ plotTurtle state pl
plotTurtle'' (TMirror pl) =
  do state <- get
     let pl' = plotTurtle state pl
     return $ Just $ pl' <^> (reverseCurve $ overCurvePoints flipH pl')

-- | Convert an LR  (left right direction to an angle.
lrToAngle :: (Floating s, Num s) => LR s -> Angle s
lrToAngle L       = fmap negate quarterTurn
lrToAngle R       = quarterTurn
lrToAngle UTurn   = halfTurn
lrToAngle (Arb a) = a

-- | A list of turtle moves.
allTurtles :: (Num s) => [(String, [Turtle s])]
allTurtles =
  [ ("CurlyBracket2",[TTurn Smooth R, TGo 1, TTurn Smooth L, TTurn Hard UTurn, TTurn Smooth L, TGo 1, TTurn Smooth R, TTurn Smooth R, TGo 4, TTurn Smooth R])
  , ("CurlyBracket",[TMirror [TTurn Smooth R, TGo 1, TTurn Smooth L, TTurn Smooth L, TGo 2]])
  , ("Boxlike",take 8 $ cycle [TGo 1, TTurn Hard L])
  , ("Rounded", [TReverse $ take 8 $ cycle [TGo 5, TTurn Smooth R]])
  , ("HardTurns", [TGo 1, TTurn Hard R, TGo 1, TTurn Hard L, TGo 1])
  , ("Arc", [TTurn Smooth L])
  , ("Test", [TGo 5, TTurn Smooth L, TGo 5, TTurn Smooth R])
  , ("Circle", [TTurn Smooth L, TTurn Smooth L, TTurn Smooth L, TTurn Smooth L])
  ]

-- | Library of turle moves.
turtleLibrary' :: (Space s) => M.Map String [Turtle s]
turtleLibrary' = foldl (\m (n,s) -> M.insert n s m) M.empty allTurtles

-- | All of the names of moves in the library.
turtleNames :: [String]
turtleNames = map fst allTurtles

-- | Function to turn a list of turtle moves into an OpenCurve.
runTurtle :: (UnLoop t, Alternative t, Space s) => [Turtle s] -> OpenCurve_ t s
runTurtle = plotTurtle initialTurtleState

-- | Lookup a turtle move in the library.
turtleLibrary :: (Space s) => String -> Maybe [Turtle s]
turtleLibrary n = M.lookup n turtleLibrary'

-- | Lookup a curve in the library based on the name.
curveLibrary :: (UnLoop t, Alternative t, Space s) => String -> Maybe (OpenCurve_ t s)
curveLibrary n = fmap runTurtle  (turtleLibrary n)

-- | Cycle a turtle move reps number of times.
cyclic :: [Turtle s] -> Int -> [Turtle s]
cyclic pat reps = concat . take reps . repeat $ pat

-- | Create a rounded polygon with a certain number of sides.
roundedPolygon :: (UnLoop t, Alternative t, Space s) => Int -> OpenCurve_ t s
roundedPolygon sides = runTurtle . cyclic [TRadius 0.1, TGo 1, TTurn Smooth (Arb (fullTurn ^/fromIntegral sides))] $ sides

-- | Create a start with a certain number of sides.
star :: (UnLoop t, Alternative t, Space s) => Int -> Angle s -> s -> OpenCurve_ t s
star sides a r = runTurtle $ cyclic [TRadius r, TGo 1, TTurn Smooth (Arb ((fullTurn ^/ fromIntegral sides) ^+^ a)), TGo 1, TTurn Smooth (Arb (negated a))] sides

-- | Create an arc by plotting a simple turtle move.
plotArc :: (UnLoop t, Alternative t, Space s) => Angle s -> OpenCurve_ t s
plotArc angle = plotTurtle initialTurtleState [TTurn Smooth (Arb angle)]
