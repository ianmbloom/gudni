{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.ConfineTree.Out
  ( outConfineTree
  , extractConfineTreeFabrics
  , outDecoTree
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Tag

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable
import qualified Data.Set as S
import Data.List


outConfineTree :: forall m s
               .  ( Show s
                  , TreeConstraints s m
                  )
               => ConfineTagId s
               -> TreeMonad s m Doc
outConfineTree treeId =
   if treeId == nullConfineTagId
   then return $ text "X"
   else do tree <- loadConfineTag treeId
           prim <- loadTreePrim (tree ^. confineTagPrimTagId)
           less <- outConfineTree (tree ^. confineTagLessCut)
           more <- outConfineTree (tree ^. confineTagMoreCut)
           return $ ( text (show treeId) <+>
                      text "--" <+>
                      doc (tree ^. confineTagCut) <+>
                      doc (tree ^. confineTagOverhang) <+>
                      text "--" <+>
                      doc (tree ^. confineTagPrimTagId) <+>
                      doc prim
                    )
                    $$
                    nest 4 ( less $$ more)

extractConfineTreeFabrics :: forall m s
                          .  ( TreeConstraints s m
                             )
                          => ConfineTagId s
                          -> TreeMonad s m [FabricTagId]
extractConfineTreeFabrics treeId = reverse . S.toList <$> go treeId S.empty
  where
  go :: ConfineTagId s -> S.Set FabricTagId -> TreeMonad s m (S.Set FabricTagId)
  go treeId set =
       if treeId == nullConfineTagId
       then return set
       else do tree <- loadConfineTag treeId
               prim <- loadTreePrim (tree ^. confineTagPrimTagId)
               let set0 = S.insert (prim ^. primFabricTagId) set
               set1 <- go (tree ^. confineTagLessCut) set0
               go (tree ^. confineTagMoreCut) set1

outDecoTree :: forall m s
            .  ( Show s
               , Space s
               , Storable s
               , MonadIO m
               )
            => DecoTagId s
            -> TreeMonad s m Doc
outDecoTree decoId =
  if decoId == nullDecoTagId
  then return $ text "X"
  else do tree <- loadDecoTag decoId
          less <- outDecoTree (tree ^. decoTagLessCut)
          more <- outDecoTree (tree ^. decoTagMoreCut)
          return $ ( text (show decoId) <+>
                     doc (tree ^. decoTagCut      ) <+>
                     doc (tree ^. decoTagCrossings)
                   ) $$
                   nest 4 ( less $$ more )
