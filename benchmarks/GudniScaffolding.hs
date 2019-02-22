{-# LANGUAGE TemplateHaskell  #-}
module GudniScaffolding
  ( Alignment (..)
  , Align(..)
  , alignBoxes
  )
where

import Linear.V2

data Alignment = AlignOver | AlignMin | AlignMax | AlignNext deriving (Show)

data Align = Align
  { alignHorizontal :: Alignment
  , alignVertical   :: Alignment
  }

alignBoxes :: Align -> Box DisplaySpace -> Box DisplaySpace -> (Point2 DisplaySpace, Point2 DisplaySpace)
alignBoxes (Align aH aV) boxOver boxUnder =
        let (h0, h1) = case aH of
                  AlignOver -> (0,0)
                  AlignMax  -> let maxWidth = max (widthBox boxOver) (widthBox boxUnder)
                               in (maxWidth - widthBox boxOver, maxWidth - widthBox boxUnder)
                  AlignMin  -> (0,0)
                  AlignNext -> (0, widthBox boxOver)
            (v0, v1) = case aV of
                  AlignOver -> (0, 0)
                  AlignMax  -> let maxHeight = max (heightBox boxOver) (heightBox boxUnder)
                               in (maxHeight - heightBox boxOver, maxHeight - heightBox boxUnder)
                  AlignMin  -> (0,0)
                  AlignNext -> (0, heightBox boxOver)
            trans0 = makePoint h0 v0
            trans1 = makePoint h1 v1
        in  (trans0, trans1)

data Scaffolding = Coupler String
                 | Brace (Point2 DisplaySpace)
                 | Joint Scaffolding Scaffolding
                 | Transom2 (V2 String) (V2 (Point2 DisplaySpace) -> ShapeTree)
                 | Transom3 (V3 String) (V3 (Point2 DisplaySpace) -> ShapeTree)
                 | Transom4 (V4 String) (V4 (Point2 DisplaySpace) -> ShapeTree)

stack :: [Scaffolding] -> Scaffolding
stack = foldl1 (ShapeOverlap (Align AlignOver AlignNext, CombineAdd))

rack :: [CompoundTree t] -> CompoundTree t
rack = foldl1 (ShapeOverlap (Align AlignNext AlignOver, CombineAdd))

paragraph :: String -> CompoundTree RawShape
paragraph = stack . map (rack . map glyph) . lines



class Scaffoldable t where
  buildScaffolding :: t -> Scaffolding
