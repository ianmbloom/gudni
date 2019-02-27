{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Figure.Plot
  ( Plot (..)
  , (<^>)
  , expandPlot
  , plotLibrary
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
import Graphics.Gudni.Figure.Angle

import Graphics.Gudni.Util.Debug

import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens

data Plot s where
  Plot        :: [Vertex s] -> Plot s
  PlotJoin    :: Plot s -> Plot s -> Plot s
  PlotArc     :: s -> Angle s -> Plot s
  PlotSegment :: Ortho XDimension s -> Plot s
  PlotRotate  :: Angle s -> Plot s -> Plot s
  PlotScale   :: s -> Plot s -> Plot s
  PlotFlipH   :: Plot s -> Plot s
  PlotFlipV   :: Plot s -> Plot s
  PlotReverse :: Plot s -> Plot s

deriving instance (Show s) => Show (Plot s)
deriving instance (Eq s) => Eq (Plot s)
deriving instance (Ord s) => Ord (Plot s)

instance NFData s => NFData (Plot s) where
  rnf = \case
    Plot        a    -> a `deepseq`             ()
    PlotJoin    a b  -> a `deepseq` b `deepseq` ()
    PlotArc     a b  -> a `deepseq`             ()
    PlotSegment a    -> a `deepseq`             ()
    PlotRotate  a b  -> a `deepseq` b `deepseq` ()
    PlotScale   a b  -> a `deepseq` b `deepseq` ()
    PlotFlipH   a    -> a `deepseq`             ()
    PlotFlipV   a    -> a `deepseq`             ()
    PlotReverse a    -> a `deepseq`             ()

instance Hashable s => Hashable (Plot s) where
  hashWithSalt s = \case
    Plot        a    -> s `hashWithSalt` (0::Int) `hashWithSalt` a
    PlotJoin    a b  -> s `hashWithSalt` (1::Int) `hashWithSalt` a `hashWithSalt` b
    PlotArc     a b  -> s `hashWithSalt` (2::Int) `hashWithSalt` a `hashWithSalt` b
    PlotSegment a    -> s `hashWithSalt` (3::Int) `hashWithSalt` a
    PlotRotate  a b  -> s `hashWithSalt` (6::Int) `hashWithSalt` a `hashWithSalt` b
    PlotScale   a b  -> s `hashWithSalt` (7::Int) `hashWithSalt` a `hashWithSalt` b
    PlotFlipH   a    -> s `hashWithSalt` (8::Int) `hashWithSalt` a
    PlotFlipV   a    -> s `hashWithSalt` (9::Int) `hashWithSalt` a
    PlotReverse a    -> s `hashWithSalt` (10::Int) `hashWithSalt` a

(<^>) :: Plot s -> Plot s -> Plot s
(<^>) = PlotJoin

expandPlot :: (Show s, Floating s, Ord s) => Plot s -> [Vertex s]
expandPlot = expandPlot'

-- all plots should start at (0,0)
expandPlot' :: (Show s, Floating s, Ord s) => Plot s -> [Vertex s]
expandPlot' (Plot vs) = vs
expandPlot' (PlotJoin ap bp) = let as = expandPlot ap
                                   bs = expandPlot bp
                               in if null as
                                   then bs
                                   else if null bs
                                        then as
                                        else let end = last as ^. stripVertex
                                                 start = head bs ^. stripVertex
                                                 f = over stripVertex ((end ^-^ start) ^+^)
                                                 bs' = map f bs
                                             in  as ++ tail bs'
expandPlot' (PlotArc r a) = let arc = expandPlot $ PlotScale r $ makeArcPlot a
                                start = head arc ^. stripVertex
                            in  map (over stripVertex (^-^ start)) arc
expandPlot' (PlotSegment l)  = [Vert True $ makePoint 0 0, Vert True $ makePoint l 0]
expandPlot' (PlotRotate a p)= map (over stripVertex (rotate a)) $ expandPlot p
expandPlot' (PlotScale s p) = map (over stripVertex (^* s)) $ expandPlot p
expandPlot' (PlotReverse p) = reverse $ expandPlot p
expandPlot' (PlotFlipH p) = map (\ (Vert o p) -> Vert o (makePoint (negate $ p ^. pX)  (p ^. pY))) (expandPlot p)
expandPlot' (PlotFlipV p) = map (\ (Vert o p) -> Vert o (makePoint (p ^. pX) (negate $ p ^. pY) )) (expandPlot p)

makeArc :: (Show s, Floating s, Num s) => Angle s -> [Vertex s]
makeArc angle = [Vert True (Point2 1 0), Vert False (Point2 1 (tanA $ fmap (/2) angle)), Vert True (Point2 (cosA angle) (sinA angle))]

makeArcPlot :: (Show s, Ord s, Floating s) => Angle s -> Plot s
makeArcPlot angle
    | abs (angle ^. deg) < 45 = Plot $ makeArc angle
    | abs (angle ^. deg) == 0 = Plot []
    | otherwise =
      makeArcPlot (angle ^/ 2) <^> PlotRotate (angle ^/ 2) (makeArcPlot (angle ^/ 2))

data Turn = Hard | Smooth deriving (Ord, Eq, Show)
data LR s = L | R | UTurn | Arb (Angle s) deriving (Ord, Eq, Show)

data Turtle s where
  TGo :: s -> Turtle s
  TRadius :: s -> Turtle s
  TTurn :: Turn -> LR s -> Turtle s
  TVerts :: s -> [Vertex s] -> Turtle s
  TReverse :: [Turtle s] -> Turtle s
  TMirror :: [Turtle s] -> Turtle s
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

turtleToPlot :: (Real s, Ord s, Floating s) => TurtleState s -> [Turtle s] -> Plot s
turtleToPlot state ss = let (m_plots, s) = runState (mapM turtleToPlot' ss) initialTurtleState
                            plots = catMaybes m_plots
                        in foldl1 (<^>) plots

turtleToPlot' :: forall s . (Real s, Ord s, Floating s) => Turtle s -> State (TurtleState s) (Maybe (Plot s))
turtleToPlot' simp =
  do
    a  <- use turtleAngle
    m_plot <- turtleToPlot'' simp
    case m_plot of
      Nothing -> return Nothing
      Just plot -> return $ Just $ PlotRotate a plot


turtleToPlot'' :: (Real s, Ord s, Floating s) => Turtle s -> State (TurtleState s) (Maybe (Plot s))
turtleToPlot'' (TGo distance) = do r <- use turtleRadius ; return $ Just $ PlotSegment $ toXOrtho distance
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
          let f = if a > (0 @@ deg) then PlotRotate (-90 @@ deg) else PlotRotate (90 @@ deg)
          return $ Just $ f $ PlotArc r a
turtleToPlot'' (TVerts d vs) =
  if length vs < 2
  then
    error "turtleToPlot, verts < 2" -- plotPlot' p
  else
    do
      let f = head vs ^. stripVertex
          l = last vs ^. stripVertex
          mag = norm (f ^-^ l)
          scale = d / mag
      return $ Just $ PlotScale scale $ Plot vs
turtleToPlot'' (TReverse pl) =
  do state <- get
     return $ Just $ PlotReverse $ turtleToPlot state pl
turtleToPlot'' (TMirror pl) =
  do state <- get
     let pl' = turtleToPlot state pl
     return $ Just $ pl' <^> (PlotReverse $ PlotFlipV pl')

lrToAngle :: (Floating s, Num s) => LR s -> Angle s
lrToAngle L       = fmap negate quarterTurn
lrToAngle R       = quarterTurn
lrToAngle UTurn   = halfTurn
lrToAngle (Arb a) = a

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

turtleLibrary' :: (Num s) => M.Map String [Turtle s]
turtleLibrary' = foldl (\m (n,s) -> M.insert n s m) M.empty allTurtles

turtleNames :: [String]
turtleNames = map fst allTurtles

runTurtle :: (Real s, Ord s, Floating s) => [Turtle s] -> Plot s
runTurtle = turtleToPlot initialTurtleState

turtleLibrary :: (Num s) => String -> Maybe [Turtle s]
turtleLibrary n = M.lookup n turtleLibrary'

plotLibrary :: (Real s, Ord s, Floating s) => String -> Maybe (Plot s)
plotLibrary n = fmap runTurtle  (turtleLibrary n)

cyclic pat reps = take (reps * length pat) . cycle $ pat

roundedPolygon :: (Show s, Real s, Fractional s, Num s, Floating s) => Int -> Plot s
roundedPolygon sides = runTurtle . cyclic [TRadius 0.1, TGo 1, TTurn Smooth (Arb (fullTurn ^/fromIntegral sides))] $ sides

star :: (Show s, Real s, Fractional s, Num s, Floating s) => Int -> Angle s -> s -> Plot s
star sides a r = runTurtle $ cyclic [TRadius r, TGo 1, TTurn Smooth (Arb ((fullTurn ^/ fromIntegral sides) ^+^ a)), TGo 1, TTurn Smooth (Arb (negated a))] sides

plotArc :: (Real s, Floating s) => Angle s -> Plot s
plotArc angle = turtleToPlot initialTurtleState [TTurn Smooth (Arb angle)]
