{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Layout.Text
  ( paragraph
  , blurb
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Adjacent
import Data.Char (ord)

import Control.Monad.State

paragraphOf :: forall token s style
            .  ( Space s
               , IsStyle style)
            => style
            -> String
            -> LayoutCompound s style
paragraphOf style string =
    let alignY = styleTextAlignY style
        stringLines = lines string
    in  stackOf style alignY defaultValue $ map (blurbOf style) stringLines

paragraph :: forall token s style
          .  ( Space s
             , IsStyle style
             )
          => String
          -> LayoutCompound s style
paragraph = paragraphOf defaultValue

blurbOf :: forall token s style
           .  ( Space s
              , IsStyle style
              )
           => style
           -> String
           -> LayoutCompound s style
blurbOf style string =
    let alignX = styleTextAlignX style
        glyphs = map (glyphOf style) string
    in  rackOf style alignX defaultValue $ glyphs

blurb :: forall token s style
      .  ( Space s
         , IsStyle style
         )
      => String
      -> LayoutCompound s style
blurb = blurbOf defaultValue

glyphOf :: style -> Char -> LayoutCompound s style
glyphOf style = SLeaf . Glyph style . CodePoint . ord
