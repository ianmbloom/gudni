{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeFamilies        #-}

module Graphics.Gudni.Figure.Cut
  ( findCutBezier
  , maybeCutPointBezier
  , CanCut(..)
  )
where

import Linear

import Graphics.Gudni.Figure.Axis
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

-- | Find the t parameter that cuts a bezier at verticle line x
findCutBezier :: (Space s, Axis axis) => axis -> s -> Bezier s -> s
findCutBezier axis cut bezier =
    if (bezier ^. bzStart . with axis <= bezier ^. bzEnd . with axis)
    then findSplit (\t ->         cut - (view (bzControl . with axis) . insideBezier t $ bezier ))
    else findSplit (\t -> negate (cut - (view (bzControl . with axis) . insideBezier t $ bezier)))

maybeCutPointBezier :: (Space s, Axis axis) => axis -> s -> Bezier s -> Maybe s
maybeCutPointBezier axis split bz =
  if canCut axis split bz
  then Just $ findCutBezier axis split bz
  else Nothing

class (HasSpace t) => CanCut t where
   -- | Split item across horizontal or vertical line
   splitAtCut :: Axis axis => axis -> SpaceOf t -> t -> (t, t)
   -- | Determine if horizontal or vertical line cuts item
   canCut     :: Axis axis => axis -> SpaceOf t -> t -> Bool

instance Space s => CanCut (Bezier s) where
   splitAtCut axis cut bezier =
     let splitT = findCutBezier axis cut bezier
         (left,right) = splitBezier splitT bezier
         -- correct the split to be exactly the same as the cutpoint
     in  (set (bzEnd . with axis) cut left, set (bzStart . with axis) cut right)
   canCut axis splitPoint bz =
       let start = bz ^. bzStart . with axis
           end   = bz ^. bzEnd   . with axis
       in  splitPoint > min start end && splitPoint < max start end
