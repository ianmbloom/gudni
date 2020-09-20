
module Graphics.Gudni.Layout.Mask
  ( CanMask(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.ShapeTree
--
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Layout
-- import Graphics.Gudni.Layout.Fill
--
-- import Graphics.Gudni.Util.Util
-- import Graphics.Gudni.Util.Plot
--
-- import Graphics.Gudni.Raster.TraverseShapeTree
--
-- import Graphics.Gudni.Layout.Collect
-- import Graphics.Gudni.Util.Debug
--
-- import Control.Lens
-- import qualified Data.Vector as V
-- import Control.Applicative

class HasSpace t => CanMask t where
    mask :: Shape (SpaceOf t) -> t

instance Space s => CanMask (CompoundTree s) where
    mask = CompoundTree . SLeaf . SItem . Just

instance IsStyle style => CanMask (CompoundLayout style) where
    mask = CompoundLayout . SLeaf . SItem . Just . LayoutShape
