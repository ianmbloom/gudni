{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Draw.Text
  ( paragraphOf
  , paragraph
  , blurbOf
  , blurb
  , glyphOf
  , glyph
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Fabric.Type

import Graphics.Gudni.Layout.Class
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Collect
import Graphics.Gudni.Layout.Type

import Data.Char (ord)
import Control.Monad.State

paragraphOf :: forall style
            .  ( IsStyle style)
            => style
            -> String
            -> Layout Mono style
paragraphOf style string =
    let alignY = styleTextAlignY style
        stringLines = lines string
    in  stackOf style alignY defaultValue $ map (blurbOf style) stringLines

paragraph :: forall style
          .  ( IsStyle style
             )
          => String
          -> Layout Mono style
paragraph = paragraphOf defaultValue

blurbOf :: forall style
           .  ( IsStyle style
              )
           => style
           -> String
           -> Layout Mono style
blurbOf style string =
    let alignX = styleTextAlignX style
        glyphs = map (glyphOf style) string
    in  rackOf style alignX defaultValue $ glyphs

blurb :: forall style
      .  ( IsStyle style
         )
      => String
      -> Layout Mono style
blurb = blurbOf defaultValue

glyphOf :: style -> Char -> Layout Mono style
glyphOf style char = Layout . FLeaf . GlyphLeaf $ (style, CodePoint . ord $ char)

glyph :: HasDefault style => Char -> Layout Mono style
glyph = glyphOf defaultValue
