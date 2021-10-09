{-# LANGUAGE ScopedTypeVariables  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Out
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for pretty printing the fabric structure after it has been
-- serialized.

module Graphics.Gudni.Raster.Fabric.Out
  ( outFabric
  , simpleOutFabric
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle

import Graphics.Gudni.Raster.ConfineTree.Out
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.Storage

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Control.Monad
import Control.Monad.IO.Class

import qualified Text.PrettyPrint as P

outFabric :: forall m s
          .  ( Show s
             , Out s
             , DagConstraints s m
             )
          => DagMonad s m Doc
outFabric =
    do start <- fromIntegral . unFabricTagId <$> fabricCodeStart
       go start
       where
       go :: Int
          -> DagMonad s m Doc
       go cursor =
         do do tag <- loadFabricTagS (FabricTagId . fromIntegral $ cursor)
               dc <- (text (rpad 4 $ show cursor) <+>) <$> marshallTag tag
               (dc $$) <$> if fabTagIsReturn tag
                           then return $ text "-----------"
                           else go (cursor - 1)
       textTag tag = text (show tag)
       hangTag tag = hang (textTag tag) 4
       jumpFab mess tagId = hang (text mess <+> text (show tagId) <+> text "-->") 4 <$> go (fromIntegral . unFabricTagId $ tagId)
       marshallTag tag
           | fabTagIsReturn      tag = return $ textTag tag
           | fabTagIsConstant    tag = do color <- loadColorS tag
                                          return $ textTag tag <+> text (show color)
           | fabTagIsTexture     tag = return $ textTag tag
           | fabTagIsFunction    tag = return $ textTag tag
           | fabTagIsBinary      tag = return $ textTag tag
           | fabTagIsDecoTree    tag = do decoOut <- return $ text "..." -- inTree $ outDecoTree (fabTagDecoId tag)
                                          return $ hangTag tag decoOut
           | fabTagIsConfineTree tag = do let confineId = fabTagConfineId tag
                                          confineOut <- return $ text "..." -- inTree $ outConfineTree confineId
                                          fabricList <- inTree $ extractConfineTreeFabrics confineId
                                          primFabrics <- mapM (jumpFab "T") fabricList
                                          return $ hangTag tag (confineOut
                                                                $$
                                                                vcat primFabrics)
           | fabTagIsStacker     tag = hangTag tag <$> (jumpFab "S") (fabTagStackerId tag)
           | fabTagIsAffine      tag = do  affine <- loadAffineS tag
                                           return $ hangTag tag $ doc affine
           | fabTagIsFacet       tag = do  facet <- loadFacetS tag
                                           return $ hangTag tag $ doc facet
           | fabTagIsConvolve    tag = do  scale <- loadConvolveS tag
                                           return $ hangTag tag $ doc scale

simpleOutFabric :: forall m s
                .  ( Show s
                   , Out s
                   , DagConstraints s m
                   )
                => DagMonad s m Doc
simpleOutFabric =
    do start <- fromIntegral . unFabricTagId <$> fabricCodeStart
       go start
       where
       go :: Int
          -> DagMonad s m Doc
       go cursor =
         if cursor < 0
         then return $ text "^^^ ^^^"
         else do tag <- loadFabricTagS (FabricTagId . fromIntegral $ cursor)
                 let tTag = text (rpad 4 $ show cursor) <+> textTag tag $$
                            if fabTagIsReturn tag
                            then text "--- ---"
                            else P.empty
                 (tTag $$) <$> go (cursor - 1)
       textTag tag = text (show tag)