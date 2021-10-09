{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Pull
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Pull intrinsically defined fabrics such as glyphs and images out of a fabric.

module Graphics.Gudni.Raster.Fabric.Pull
  ( pullGlyphs
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Font

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Combine.Tag
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Substance.Storage
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Filter.Tag
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Transformer.Storage
import Graphics.Gudni.Raster.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.Fabric.FromLayout
import Graphics.Gudni.Raster.Storage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Lens
import Foreign.Storable

import qualified Data.Vector     as V
import qualified Data.Map.Strict as M

data PullPass var style

instance HasSpace style => HasSpace (PullPass var style) where
    type SpaceOf (PullPass var style) = SpaceOf style

instance IsStyle style => HasStyle (PullPass var style) where
    type StyleOf (PullPass var style) = style

instance (HasSpace style, Ord var) => FabricType (PullPass var style) where
    type FChildType  (PullPass var style) = Fabric (PullPass var style)
    type FBinaryType (PullPass var style) = ProximityMeld style FCombineType
    type FPostType   (PullPass var style) = FFilter
    type FPreType    (PullPass var style) = FTransformer (SpaceOf style)
    type FLeafType   (PullPass var style) = GlyphLeaf (FontName, CodePoint) (PullPass var style)
    type FVarName    (PullPass var style) = var

instance HasSpace style => SubstanceType (PullPass var style) where
    type FTex        (PullPass var style) = PictureMemoryReference
    type FQuery      (PullPass var style) = Color (SpaceOf style)

data PullState var style
    = PullState
    { _glyphFabricMap :: M.Map (style, CodePoint) (Fabric (PullPass var style))
    }
makeLenses ''PullState

type PullMonad var style m a = StateT (PullState var style) m a

pullGlyphs :: forall i var style m
           .  ( Monad m
              , IsStyle style
              , Ord var
              )
           => Fabric (PicturePass var style)
           -> FontMonad (SpaceOf style) m (M.Map (style, CodePoint) (Fabric (PullPass var style)))
pullGlyphs fabric =
    view glyphFabricMap <$> execStateT (go fabric) (PullState M.empty)
    where
    go :: Fabric (PicturePass var style)
       -> PullMonad var style (FontMonad (SpaceOf style) m) ()
    go fabric =
        case fabric of
            FDefine varName body applied ->
                do go applied
                   go body
            FVar varName -> return ()
            FBinary ty above below ->
                do go below
                   go above
            FLeaf glyphLeaf ->
                case glyphLeaf of
                    GlyphLeaf glyph ->
                     do glMap <- use glyphFabricMap
                        case M.lookup glyph glMap of
                            Just fab -> return ()
                            Nothing -> do loadedGlyph <- lift $ loadGlyphs (styleGlyph glyph)
                                          glyphFabricMap .= M.insert glyph loadedGlyph glMap
                    ShapeLeaf shapeLeaf -> return ()
            FUnaryPre  trans child -> go child
            FUnaryPost filt  child -> go child

loadGlyphs :: ( Monad m
              , HasSpace i
              , FChildType i ~ Fabric i
              , FChildType j ~ Fabric j
              , FPreType  i ~ FPreType  j
              , FPostType i ~ FPostType j
              , FVarName  i ~ FVarName  j
              , FBinaryType i ~ FBinaryType j
              , FLeafType i ~ GlyphLeaf (String, CodePoint) i
              , FLeafType j ~ GlyphLeaf (String, CodePoint) i
              ) => Fabric i -> FontMonad (SpaceOf i) m (Fabric j)
loadGlyphs =
    go
    where
    go fabric =
       case fabric of
           FDefine varName body applied ->
               do applied' <- go applied
                  body' <- go body
                  return $ FDefine varName body' applied'
           FVar varName -> return $ FVar varName
           FBinary ty above below ->
               do below' <- go below
                  above' <- go above
                  return $ FBinary ty above' below'
           FLeaf glyphLeaf ->
               case glyphLeaf of
                   GlyphLeaf (fontName, codePoint) ->
                       FLeaf . ShapeLeaf . FShape <$> getGlyph fontName codePoint
                   ShapeLeaf shapeLeaf -> return . FLeaf . ShapeLeaf $ shapeLeaf
           FUnaryPre  trans child -> FUnaryPre  trans <$> go child
           FUnaryPost filt  child -> FUnaryPost filt  <$> go child
