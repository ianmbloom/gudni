{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
module Graphics.Gudni.Figure.Cut
  ( findCutBezier
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
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative

-- | Find the t parameter that cuts a bezier at verticle line x
findCutBezier :: Space s => Lens' (Point2 s ) s -> s -> Bezier s -> s
findCutBezier axis cut bezier =
    if (bezier ^. bzStart . axis <= bezier ^. bzEnd . axis)
    then findSplit (\t ->         cut - (view (bzControl . axis) . insideBezier t $ bezier ))
    else findSplit (\t -> negate (cut - (view (bzControl . axis) . insideBezier t $ bezier)))

maybeCutPointBezier :: Space s => Lens' (Point2 s) s -> s -> Bezier s -> Maybe s
maybeCutPointBezier axis split bz =
  if canCut axis split bz
  then Just $ findCutBezier axis split bz
  else Nothing

class (HasSpace t, Show (SpaceOf t)) => CanCut t where
   -- | Split item across horizontal or vertical line
   splitAtCut    :: Lens' (Point2 (SpaceOf t)) (SpaceOf t) -> SpaceOf t -> t -> (t, t)
   -- | Determine if horizontal or vertical line cuts item
   canCut        :: Lens' (Point2 (SpaceOf t)) (SpaceOf t) -> SpaceOf t -> t -> Bool

instance Space s => CanCut (Bezier s) where
   splitAtCut axis cut bezier =
     let splitT = findCutBezier axis cut bezier
         (left,right) = splitBezier splitT bezier
         -- correct the split to be exactly the same as the cutpoint
     in  (set (bzEnd . axis) cut left, set (bzStart . axis) cut right)
   canCut axis splitPoint bz =
       let start = bz ^. bzStart . axis
           end   = bz ^. bzEnd   . axis
       in  splitPoint > min start end && splitPoint < max start end
