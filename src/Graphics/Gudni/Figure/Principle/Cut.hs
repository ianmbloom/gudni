{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

module Graphics.Gudni.Figure.Principle.Cut
    ( CanCut(..)
    )
where

import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.Box

import Control.Lens

class (HasSpace t) => CanCut t where
   -- | Split item across horizontal or vertical line
   splitAtCut :: Axis axis => axis -> Athwart axis (SpaceOf t) -> t -> (t, t)
   -- | Determine if horizontal or vertical line cuts item
   canCut     :: Axis axis => axis -> Athwart axis (SpaceOf t) -> t -> Bool

instance Space s => CanCut (Box s) where
   splitAtCut = splitBox
   canCut axis splitPoint box = splitPoint > box ^. minBox . athwart axis && splitPoint < box ^. maxBox . athwart axis
