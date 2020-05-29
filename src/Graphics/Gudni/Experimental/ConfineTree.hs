{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Gudni.Experimental.ConfineTree
  ( Tack(..)
  , tackLeft
  , tackAbove
  , ConfineTree(..)
  , confineTack
  , confinePosition
  , confineSubstance
  , confineCurve
  , confineInside
  , confineOutside
  , Layer(..)
  , layerSubstance
  , layerInside
  , layerCurve
  , SimpleLayer(..)
  , simpSubstance
  , simpInverse

  , maxBoundaries
  , CanConfine(..)
  , insideBoundaryPoint
  , outsideBoundaryPoint
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Experimental.TreeOrderTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Util.Util(breakVector)
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import qualified Data.Vector as V

-- | Tack is a synonym for the orientation of a confinement branch
-- left  == True   ..  left  == True   ..  left  == False  ..  left  == False  ..
-- above == True   ..  above == False  ..  above == True   ..  above == False  ..
--         \       ..                  ..          \       ..                  ..
--    In   \       ..         Out      ..          \ In    ..         Out      ..
--         \       ..                  ..          \       ..                  ..
--  -------        ..   -------        ..          ------- ..          ------- ..
--                 ..          \       ..                  ..         \        ..
--        Out      ..     In   \       ..         Out      ..         \  In    ..
--                 ..          \       ..                  ..         \        ..
--
data Tack =
  Tack
  { _tackLeft  :: Bool
  , _tackAbove :: Bool
  } deriving (Show, Eq)
makeLenses ''Tack

type SubstanceTagId = Int

data ConfineTree s =
  ConfineBranch
  { _confineTack      :: Tack
  , _confinePosition  :: Point2 s
  , _confineSubstance :: SubstanceTagId
  , _confineCurve     :: Bezier s
  , _confineInside    :: Maybe (ConfineTree s)
  , _confineOutside   :: Maybe (ConfineTree s)
  } deriving (Show)
makeLenses ''ConfineTree

--instance Show s => Show (ConfineTree s) where
--  show branch = show (branch ^. confineTack) ++ " " ++ show (branch ^. confinePosition)

pointIsInside :: Ord s => Tack -> Point2 s -> Point2 s -> Bool
pointIsInside tack branchPosition point =
   let horiz = if tack ^. tackLeft
               then point ^. pX < branchPosition ^. pX
               else point ^. pX > branchPosition ^. pX
       vert  = if tack ^. tackAbove
               then point ^. pY < branchPosition ^. pY
               else point ^. pY > branchPosition ^. pY
   in  horiz && vert


pointIsInsideBranch :: Ord s => ConfineTree s -> Point2 s -> Bool
pointIsInsideBranch branch = pointIsInside (branch ^. confineTack) (branch ^. confinePosition)

data Layer s =
  Layer
  { _layerSubstance :: SubstanceTagId
  , _layerInside    :: Bool
  , _layerCurve     :: Bezier s
  } deriving (Show)
makeLenses ''Layer

confinementStack :: Ord s => ConfineTree s -> Point2 s -> [Layer s]
confinementStack branch point =
  let inside = pointIsInsideBranch branch point
      insideCase  = case branch ^. confineInside of
                        Just insideBranch -> confinementStack insideBranch point
                        Nothing -> []
      outsideCase = case branch ^. confineOutside of
                        Just outsideBranch -> confinementStack outsideBranch point
                        Nothing -> []
      result = if inside then insideCase else outsideCase
  in  Layer (branch ^. confineSubstance) inside (branch ^. confineCurve):result

isVertical :: Eq s => Bezier s -> Bool
isVertical bez = bez ^. bzStart . pX == bez ^. bzEnd . pX

isAboveCurve :: Space s => Point2 s -> Bezier s ->  Bool
isAboveCurve point bezier =
  if isVertical bezier
  then True
  else let t = findCutBezier pX (point ^. pX) bezier
           bPoint = eval t bezier
       in  point ^. pY < bPoint ^. pY

data SimpleLayer =
  SimpleLayer
  { _simpSubstance :: SubstanceTagId
  , _simpInverse   :: Bool
  } deriving (Show)
makeLenses ''SimpleLayer

simplifyLayer :: Space s => Point2 s -> Layer s -> SimpleLayer
simplifyLayer point layer =
  let aboveCurve = isAboveCurve point (layer ^. layerCurve)
  in  SimpleLayer (layer ^. layerSubstance) (aboveCurve == layer ^. layerInside)

sampleStack :: Space s => Point2 s -> [Layer s] -> [SimpleLayer]
sampleStack point = map (simplifyLayer point) . sortWith (view layerSubstance)

goesRight :: Ord s => Bezier s -> Bool
goesRight bez = bez ^. bzStart . pX <= bez ^. bzEnd . pX

goesDown  :: Ord s => Bezier s -> Bool
goesDown  bez = bez ^. bzStart . pY <= bez ^. bzEnd . pY

tack :: Ord s => Bezier s -> Tack
tack bez = Tack { _tackLeft  = not (goesDown bez)
                , _tackAbove = goesRight bez
                }

outsideBoundaryPoint :: Ord s => Bezier s -> Point2 s
outsideBoundaryPoint bez =
    let x = if goesDown bez /= goesRight bez then bez ^. bzStart . pX else bez ^. bzEnd   . pX
        y = if goesDown bez == goesRight bez then bez ^. bzStart . pY else bez ^. bzEnd   . pY
    in  Point2 x y

insideBoundaryPoint :: Ord s => Bezier s -> Point2 s
insideBoundaryPoint bez =
    let x = if goesDown bez == goesRight bez then bez ^. bzStart . pX else bez ^. bzEnd . pX
        y = if goesDown bez /= goesRight bez then bez ^. bzStart . pY else bez ^. bzEnd . pY
    in  Point2 x y

makeConfinement :: Space s => SubstanceTagId -> Bezier s -> ConfineTree s
makeConfinement substance bez =
     ConfineBranch
     { _confineTack      = tack bez
     , _confinePosition  = insideBoundaryPoint bez
     , _confineSubstance = substance
     , _confineCurve     = bez
     , _confineInside    = Nothing
     , _confineOutside   = Nothing
     }

oppositeTack :: Tack -> Tack
oppositeTack (Tack left above) = Tack (not left) (not above)

maxBoundaries :: Space s => Box s
maxBoundaries = Box (Point2 minBound minBound) (Point2 maxBound maxBound)

addConfineTree :: Space s => Maybe (ConfineTree s) -> ConfineTree s -> Maybe (ConfineTree s)
addConfineTree mOld new = go mOld new
  where
  go mOld new =
      case mOld of
        Nothing -> Just new
        Just old ->
           let nTack = new ^. confineTack
               oTack = old ^. confineTack
               outsidePointConfined = {-tr ("outsidePointConfined old:" ++ show old ++ " new:" ++ show new) $-} pointIsInsideBranch old (outsideBoundaryPoint (new ^. confineCurve))
               insidePointConfined  = {-tr ("insidePointConfined  old:" ++ show old ++ " new:" ++ show new) $-} pointIsInsideBranch old (insideBoundaryPoint  (new ^. confineCurve))
               setInside    = if outsidePointConfined || insidePointConfined
                              then set confineInside (go (old ^. confineInside) new)
                              else id
               setOutside   = if not outsidePointConfined || not insidePointConfined
                              then set confineOutside (go (old ^. confineOutside) new)
                              else id
           in  Just . setInside . setOutside $ old


outlineToConfinements :: (Space s) => SubstanceTagId -> Outline s -> V.Vector (ConfineTree s)
outlineToConfinements substance =
    V.map (makeConfinement substance) .
    join .
    fmap (replaceKnob horizontalAxis) .
    join .
    fmap (replaceKnob verticalAxis) .
    view outlineSegments

class HasSpace t => CanConfine t where
  addToConfineTree :: TreeOrderTable
                   -> Int
                   -> SubstanceTagId
                   -> Maybe (ConfineTree (SpaceOf t))
                   -> t
                   -> Maybe (ConfineTree (SpaceOf t))

instance Space s => CanConfine (Shape s) where
    addToConfineTree table maxSize substance tree =
        --foldl (V.foldl (treeOrderFold table addConfineTree)) tree .
        --map (breakVector maxSize . outlineToConfinements substance) .
        --view shapeOutlines
        V.foldl addConfineTree tree .
        V.concat .
        map (outlineToConfinements substance) .
        view shapeOutlines

instance Space s => CanConfine (ShapeTree Int s) where
    addToConfineTree table maxSize substance tree =
        foldl (addToConfineTree table maxSize substance) tree . flattenShapeTree
