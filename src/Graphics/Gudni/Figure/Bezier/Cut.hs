{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}

module Graphics.Gudni.Figure.Bezier.Cut
  ( findCutBezier
  , maybeCutPointBezier
  )
where

import Linear

import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.Box
import Graphics.Gudni.Figure.Principle.Cut
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Figure.Bezier.Split
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Base.Chain
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative
import Text.PrettyPrint.GenericPretty

-- | Find the t parameter that cuts a bezier at verticle line x
findCutBezier :: (Space s, Axis axis) => axis -> Athwart axis s -> Bezier s -> s
findCutBezier axis cut bezier =
    if (bezier ^. bzStart . athwart axis <= bezier ^. bzEnd . athwart axis)
    then findSplit (\t -> fromAthwart axis $         cut - (view (bzControl . athwart axis) . insideBezier t $ bezier ))
    else findSplit (\t -> fromAthwart axis $ negate (cut - (view (bzControl . athwart axis) . insideBezier t $ bezier)))

maybeCutPointBezier :: (Space s, Axis axis) => axis -> Athwart axis s -> Bezier s -> Maybe s
maybeCutPointBezier axis split bz =
  if canCut axis split bz
  then Just $ findCutBezier axis split bz
  else Nothing

instance Space s => CanCut (Bezier s) where
   splitAtCut axis cut bezier =
     let splitT = findCutBezier axis cut bezier
         (left,right) = splitBezier splitT bezier
         -- correct the split to be exactly the same as the cutpoint
     in  (set (bzEnd . athwart axis) cut left, set (bzStart . athwart axis) cut right)
   canCut axis splitPoint bz =
       let start = bz ^. bzStart . athwart axis
           end   = bz ^. bzEnd   . athwart axis
       in  splitPoint > min start end && splitPoint < max start end
