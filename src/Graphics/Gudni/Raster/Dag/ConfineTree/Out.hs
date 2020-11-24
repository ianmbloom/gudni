{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Out
  ( outConfineTree
  , outDecoTree
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Storage

-- import Graphics.Gudni.Raster.Serial.Slice

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable


outConfineTree :: forall m s
               .  ( Show s
                  , Space s
                  , Storable s
                  , MonadIO m
                  )
               => ConfineTagId s
               -> DagMonad s m Doc
outConfineTree treeId =
   if treeId == nullConfineTagId
   then return $ text "X"
   else do tree <- loadTreeTagS treeId
           less <- outConfineTree (tree ^. confineTagLessCut)
           more <- outConfineTree (tree ^. confineTagMoreCut)
           return $ ( text (show treeId) <+> text "prim" <+>
                      text (show (tree ^. confineTagPrimTagId)) <+>
                      text "--" <+>
                      doc (tree ^. confineTagCut) <+>
                      doc (tree ^. confineTagOverhang)
                    )
                    $$
                    nest 4 ( less $$ more)

outDecoTree :: forall m s
            .  ( Show s
               , Space s
               , Storable s
               , MonadIO m
               )
            => DecoTagId s
            -> DagMonad s m Doc
outDecoTree decoId =
  if decoId == nullDecoTagId
  then return $ text "X"
  else do tree <- loadDecoTagS decoId
          less <- outDecoTree (tree ^. decoTagLessCut)
          more <- outDecoTree (tree ^. decoTagMoreCut)
          return $ ( text (show decoId) <+>
                     doc (tree ^. decoTagCut      ) <+>
                     doc (tree ^. decoTagCrossings)
                   ) $$
                   nest 4 ( less $$ more )
