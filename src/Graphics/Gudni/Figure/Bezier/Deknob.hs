{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module Graphics.Gudni.Figure.Bezier.Deknob
  ( findKnobSplit
  , maybeKnobSplitPoint
  )
where

import Linear

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Axis
import Graphics.Gudni.Figure.Primitive.Point
import Graphics.Gudni.Figure.Primitive.Box
import Graphics.Gudni.Figure.Primitive.Bezier
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Deknob.Class
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative

findKnobSplit :: (Axis axis, Space s) => axis -> Bezier s -> s
findKnobSplit axis bz = findSplit (splitDirection axis bz)

maybeKnobSplitPoint :: (Axis axis, Space s) => axis -> Bezier s -> Maybe s
maybeKnobSplitPoint axis bz =
   if isKnob axis bz
   then Just $ findKnobSplit axis bz
   else Nothing

instance Space s => CanDeknob (Bezier s) where
    -- | If a curve is a knob, split it.
    deKnob axis bz@(Bez v0 control v1) =
      fmap (splitChain axis bz) ({-tr ("maybeDeKnob " ++ show bz) $-} maybeKnobSplitPoint axis bz)
