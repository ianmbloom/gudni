{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Gudni.Util.Plot
  ( plotLibrary
  , allTurtles
  , turtleNames
  , turtleToPlot
  , roundedPolygon
  , plotArc
  , star
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformer

import Graphics.Gudni.Util.Debug

import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens

makeArc :: (Show s, Floating s, Num s) => Angle s -> OpenCurve s
makeArc angle = OpenCurve [curved 1 0 1 (Ortho $ tanA $ angle ^/ 2)] (Point2 (cosA angle) (sinA angle))

makeArcPlot :: Angle DisplaySpace -> OpenCurve DisplaySpace
makeArcPlot angle
    | abs (angle ^. deg) < 45 = makeArc angle
    | abs (angle ^. deg) == 0 = OpenCurve [] zeroPoint
    | otherwise =
      makeArcPlot (angle ^/ 2) <^> overCurve (tRotate (angle ^/ 2)) (makeArcPlot (angle ^/ 2))

data Turn = Hard | Smooth deriving (Ord, Eq, Show)
data LR s = L | R | UTurn | Arb (Angle s) deriving (Ord, Eq, Show)

-- | Simple data structure for creating shapes by moving a turtle.
data Turtle where
  TGo :: DisplaySpace -> Turtle
  TRadius :: DisplaySpace -> Turtle
  TTurn :: Turn -> LR DisplaySpace -> Turtle
  TReverse :: [Turtle] -> Turtle
  TMirror :: [Turtle] -> Turtle
  deriving (Ord, Eq, Show)

data TurtleState s = TurtleState
  { _turtleRadius :: s
  , _turtleAngle :: Angle s
  }
makeLenses ''TurtleState

initialTurtleState :: (Floating s, Num s) => TurtleState s
initialTurtleState = TurtleState 1 (0 @@ deg)

(+<+) :: (Real s, Floating s) => Angle s -> Angle s -> Angle s
(+<+) a b = normalizeAngle (a ^+^ b)

turtleToPlot :: {-(Real s, Ord s, Floating s) => -} TurtleState DisplaySpace -> [Turtle] -> OpenCurve DisplaySpace
turtleToPlot state ss = let (m_plots, s) = runState (mapM turtleToPlot' ss) initialTurtleState
                            plots = catMaybes m_plots
                        in foldl1 (<^>) plots

turtleToPlot' :: {- forall s . (Real s, Ord s, Floating s) => -} Turtle -> State (TurtleState DisplaySpace) (Maybe (OpenCurve DisplaySpace))
turtleToPlot' simp =
  do
    a  <- use turtleAngle
    m_plot <- turtleToPlot'' simp
    case m_plot of
      Nothing -> return Nothing
      Just plot -> return $ Just $ overCurve (tRotate a) plot


turtleToPlot'' :: {- (Real s, Ord s, Floating s) => -} Turtle -> State (TurtleState DisplaySpace) (Maybe (OpenCurve DisplaySpace))
turtleToPlot'' (TGo distance) = do r <- use turtleRadius ; return $ Just $ OpenCurve [straight 0 0] (makePoint (toXOrtho distance) 0)
turtleToPlot'' (TRadius r) = do turtleRadius .= r; return Nothing
turtleToPlot'' (TTurn t lr) =
  do
    let a = lrToAngle lr
    turtleAngle %= (a +<+)
    case t of
      Hard ->
        return Nothing
      Smooth ->
        do
          r <- use turtleRadius
          let f = if a > (0 @@ deg) then overCurve (tRotate (-90 @@ deg)) else overCurve (tRotate (90 @@ deg))
          return $ Just $ f $ let arc = overCurve (tScale r) $ makeArcPlot a
                                  start = head (arc ^. curveSegments) ^. anchor
                              in  overCurve (^-^ start) arc
turtleToPlot'' (TReverse pl) =
  do state <- get
     return $ Just $ reverseCurve $ turtleToPlot state pl
turtleToPlot'' (TMirror pl) =
  do state <- get
     let pl' = turtleToPlot state pl
     return $ Just $ pl' <^> (reverseCurve $ overCurve flipH pl')

flipH = over pX negate
flipV = over pY negate

lrToAngle :: (Floating s, Num s) => LR s -> Angle s
lrToAngle L       = fmap negate quarterTurn
lrToAngle R       = quarterTurn
lrToAngle UTurn   = halfTurn
lrToAngle (Arb a) = a

allTurtles :: [(String, [Turtle])]
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

turtleLibrary' :: M.Map String [Turtle]
turtleLibrary' = foldl (\m (n,s) -> M.insert n s m) M.empty allTurtles

turtleNames :: [String]
turtleNames = map fst allTurtles

runTurtle :: {-(Real s, Ord s, Floating s) =>-} [Turtle] -> OpenCurve DisplaySpace
runTurtle = turtleToPlot initialTurtleState

turtleLibrary :: String -> Maybe [Turtle]
turtleLibrary n = M.lookup n turtleLibrary'

plotLibrary :: {-(Real s, Ord s, Floating s) =>-} String -> Maybe (OpenCurve DisplaySpace)
plotLibrary n = fmap runTurtle  (turtleLibrary n)

cyclic pat reps = take (reps * length pat) . cycle $ pat

roundedPolygon :: {-(Show s, Real s, Fractional s, Num s, Floating s) =>-} Int -> OpenCurve DisplaySpace
roundedPolygon sides = runTurtle . cyclic [TRadius 0.1, TGo 1, TTurn Smooth (Arb (fullTurn ^/fromIntegral sides))] $ sides

star :: {-(Show s, Real s, Fractional s, Num s, Floating s) =>-} Int -> Angle DisplaySpace -> DisplaySpace -> OpenCurve DisplaySpace
star sides a r = runTurtle $ cyclic [TRadius r, TGo 1, TTurn Smooth (Arb ((fullTurn ^/ fromIntegral sides) ^+^ a)), TGo 1, TTurn Smooth (Arb (negated a))] sides

plotArc :: {-(Real s, Floating s) =>-} Angle DisplaySpace -> OpenCurve DisplaySpace
plotArc angle = turtleToPlot initialTurtleState [TTurn Smooth (Arb angle)]
