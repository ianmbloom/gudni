{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Layout.Text
  ( paragraph
  , blurb
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Adjacent

import Control.Monad.State

paragraph :: forall m . (MonadState FontCache m, Monad m)
          => X SubSpace
          -> Y SubSpace
          -> Alignment
          -> Alignment
          -> String
          -> m (Glyph (CompoundTree SubSpace))
paragraph gapX gapY alignX alignY string =
  do  let stringLines = lines string
      glyphLines <- mapM glyphString stringLines
      let glyphRacks = map (rack alignY . distributeRack gapX) glyphLines
      return . stack alignX . distributeStack gapY $ glyphRacks

blurb :: forall m . (MonadState FontCache m, Monad m)
      => X SubSpace
      -> Alignment
      -> String
      -> m (Glyph (CompoundTree SubSpace))
blurb gapX alignX string =
  do  glyphs <- glyphString string
      return $ rack alignX . distributeRack gapX $ glyphs
