{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Figure.Cut
  ( Axis(..)
  , isVertical
  , isHorizontal
  , findCutBezier
  , maybeCutPointBezier
  , CanCut(..)
  )
where

import Linear

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Chain
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative
import Text.PrettyPrint.GenericPretty

data Axis = Vertical | Horizontal deriving (Eq, Show, Generic)
instance Out Axis

isVertical   axis = axis == Vertical
isHorizontal axis = axis == Horizontal


-- | Find the t parameter that cuts a bezier at verticle line x
findCutBezier :: Space s => Axis -> s -> Bezier s -> s
findCutBezier axis cut bezier =
    let axisLens = if axis == Vertical then pX else pY in
    if (bezier ^. bzStart . axisLens <= bezier ^. bzEnd . axisLens)
    then findSplit (\t ->         cut - (view (bzControl . axisLens) . insideBezier t $ bezier ))
    else findSplit (\t -> negate (cut - (view (bzControl . axisLens) . insideBezier t $ bezier)))

maybeCutPointBezier :: Space s => Axis -> s -> Bezier s -> Maybe s
maybeCutPointBezier axis split bz =
  if canCut axis split bz
  then Just $ findCutBezier axis split bz
  else Nothing

class (HasSpace t, Show (SpaceOf t)) => CanCut t where
   -- | Split item across horizontal or vertical line
   splitAtCut :: Axis -> SpaceOf t -> t -> (t, t)
   -- | Determine if horizontal or vertical line cuts item
   canCut     :: Axis -> SpaceOf t -> t -> Bool

instance Space s => CanCut (Bezier s) where
   splitAtCut axis cut bezier =
     let lens  = if axis == Vertical then pX else pY
         splitT = findCutBezier axis cut bezier
         (left,right) = splitBezier splitT bezier
         -- correct the split to be exactly the same as the cutpoint
     in  (set (bzEnd . lens) cut left, set (bzStart . lens) cut right)
   canCut axis splitPoint bz =
       let lens  = if axis == Vertical then pX else pY
           start = bz ^. bzStart . lens
           end   = bz ^. bzEnd   . lens
       in  splitPoint > min start end && splitPoint < max start end
