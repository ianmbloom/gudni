{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.Dag.Fabric.Out
  ( outFabric
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.ConfineTree.Out
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Storage

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable


outFabric :: forall m s
          .  ( Show s
             , Space s
             , Storable s
             , MonadIO m
             )
          => FabricTagId
          -> DagMonad s m Doc
outFabric  fabricTagId =
    do go fabricTagId
       where
       go :: FabricTagId
          -> DagMonad s m Doc
       go fabricTagId =
              (doc fabricTagId <+>) <$>
              if fabricTagId == nullFabricTagId
              then return $ text "X"
              else do fabric <- loadFabricS fabricTagId
                      case fabric of
                        FCombine (op, shapeMinB) aboveId belowId ->
                            do aboveQ <- nest 1 <$> go aboveId
                               belowQ <- nest 1 <$> go belowId
                               return $ {-text "FCombine" <+>-} doc op <+> doc shapeMinB $$ aboveQ $$ belowQ
                        FTransform trans childId ->
                            ({-text "FTransform" <+>-} doc trans $$) <$> go childId
                        FLeaf leaf ->
                            case leaf of
                                FTree treeId child ->
                                    do (confineTreeId, decoTreeId) <- loadTreeRootS treeId
                                       confineOut <- outConfineTree confineTreeId
                                       decoOut    <- outDecoTree decoTreeId
                                       (text "FTree" <+> doc treeId
                                            $$
                                            nest 2 ( hang (text "decoTree")    4 decoOut    $$
                                                     hang (text "confineTree") 4 confineOut

                                                   )
                                            $$
                                            )
                                            <$> go child
                                FTreeSubstance substance ->
                                    return $ {- text "FTreeSubstance" <+> -} doc substance
