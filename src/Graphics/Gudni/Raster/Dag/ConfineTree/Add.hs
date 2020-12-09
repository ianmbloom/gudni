{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Add
  ( addPrimToConfineTree
  )
where

import Graphics.Gudni.Figure.Principle

import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.TagTypes

import Control.Lens

addPrimToConfineTree :: forall s m
                     . ( TreeConstraints s m
                       )
                     => Box s
                     -> PrimTagId
                     -> ConfineTagId s
                     -> TreeMonad s m (ConfineTagId s)
addPrimToConfineTree box primTagId =
    go Vertical
    where
    go :: (Axis axis)
       => axis
       -> ConfineTagId s
       -> TreeMonad s m (ConfineTagId s)
    go axis treeId =
        let minCut = box ^. minBox . athwart axis
            maxCut = box ^. maxBox . athwart axis
        in
        if treeId == nullConfineTagId
        then do let treeTag = ConfineTag { _confineTagPrimTagId  = primTagId
                                         , _confineTagHorizontal = isHorizontal axis
                                         , _confineTagCut        = fromAthwart axis minCut
                                         , _confineTagOverhang   = fromAthwart axis maxCut
                                         , _confineTagLessCut    = nullConfineTagId
                                         , _confineTagMoreCut    = nullConfineTagId
                                         }
                addConfineTag treeTag
        else do tree <- loadConfineTag treeId
                modifiedTag <- if minCut < toAthwart axis (tree ^. confineTagCut)
                               then do less <- go (perpendicularTo axis) (tree ^. confineTagLessCut)
                                       return $ over confineTagOverhang (max (fromAthwart axis maxCut)) .
                                                set confineTagLessCut less $
                                                tree
                               else do more <- go (perpendicularTo axis) (tree ^. confineTagMoreCut)
                                       return $ set confineTagMoreCut more tree
                overwriteConfineTag treeId modifiedTag
                return treeId
