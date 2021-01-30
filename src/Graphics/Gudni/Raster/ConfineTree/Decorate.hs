{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Graphics.Gudni.Raster.ConfineTree.Decorate
  ( buildDecorateTreeBruteForce
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Cross
import Graphics.Gudni.Raster.ConfineTree.Primitive.Stack

import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.ConfineTree.Traverse

import Graphics.Gudni.Raster.TagTypes

import Graphics.Gudni.Raster.Serial.Pile

import Control.Lens
import Control.Monad
import Control.Monad.State

modifyItemStackIfCrossedAlong :: ( Axis axis
                                 , Space s
                                 , Monad m
                                 )
                              => s
                              -> axis
                              -> Along axis s
                              -> Athwart axis s
                              -> Along axis s
                              -> PrimTagId
                              -> Primitive s
                              -> StateT [FabricTagId] m ()
modifyItemStackIfCrossedAlong limit lineAxis start baseline end primTagId prim =
    when (crossesPrimAlong limit lineAxis start baseline end prim) $
        modify (toggleItemActive (prim ^. primFabricTagId))

buildDecorateTreeBruteForce :: forall s m
                            .  ( TreeConstraints s m
                               )
                            => s
                            -> Int
                            -> ConfineTagId s
                            -> TreeMonad s m (DecoTagId s)
buildDecorateTreeBruteForce limit depthLimit mRoot =
    fst <$> go Vertical (toAlong Horizontal minBound) (toAlong Vertical minBound) mRoot
    where
    go :: (Axis axis, axis~PerpendicularTo(PerpendicularTo(axis)))
       => axis
       -> Athwart axis s
       -> Along   axis s
       -> ConfineTagId s
       -> TreeMonad s m (DecoTagId s, Int)
    go axis parentCut parentLine treeId =
        if treeId == nullConfineTagId
        then return (nullDecoTagId, 0)
        else do tree <- loadConfineTag treeId
                let parentAxis = perpendicularTo axis
                    cut = toAthwart axis $ tree ^. confineTagCut
                    collector primTagId =
                         do prim <- lift $ loadTreePrim primTagId
                            modifyItemStackIfCrossedAlong limit parentAxis parentCut parentLine cut primTagId prim
                crossings <- execStateT (traverseCTAlong collector
                                                         parentAxis
                                                         parentCut
                                                         parentLine
                                                         cut
                                                         mRoot
                                        ) []
                (lessTree, lessDepth) <- go (perpendicularTo axis) parentLine cut (tree ^. confineTagLessCut)
                (moreTree, moreDepth) <- go (perpendicularTo axis) parentLine cut (tree ^. confineTagMoreCut)
                let depth = 1 + max lessDepth moreDepth
                ret <- if depth <= depthLimit
                       then return nullDecoTagId
                       else do slice <- foldIntoPileS treeCrossingPile crossings
                               let treeTag = DecoTag { _decoTagCut        = tree ^. confineTagCut
                                                     , _decoTagHorizontal = isHorizontal axis
                                                     , _decoTagCrossings  = slice
                                                     , _decoTagLessCut    = lessTree
                                                     , _decoTagMoreCut    = moreTree
                                                     }
                               addDecoTag treeTag
                return (ret, depth)
