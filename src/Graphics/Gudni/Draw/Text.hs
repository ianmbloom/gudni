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
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Collect

import Data.Char (ord)
import Control.Monad.State

paragraphOf :: forall style
            .  ( IsStyle style)
            => style
            -> String
            -> CompoundLayout style
paragraphOf style string =
    let alignY = styleTextAlignY style
        stringLines = lines string
    in  stackOf style alignY defaultValue $ map (blurbOf style) stringLines

paragraph :: forall style
          .  ( IsStyle style
             )
          => String
          -> CompoundLayout style
paragraph = paragraphOf defaultValue

blurbOf :: forall style
           .  ( IsStyle style
              )
           => style
           -> String
           -> CompoundLayout style
blurbOf style string =
    let alignX = styleTextAlignX style
        glyphs = map (glyphOf style) string
    in  rackOf style alignX defaultValue $ glyphs

blurb :: forall style
      .  ( IsStyle style
         )
      => String
      -> CompoundLayout style
blurb = blurbOf defaultValue

glyphOf :: style -> Char -> CompoundLayout style
glyphOf style = CompoundLayout . SLeaf . SItem . Just . Glyph style . CodePoint . ord

glyph :: HasDefault style => Char -> CompoundLayout style
glyph = glyphOf defaultValue
