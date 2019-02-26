{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.Scaffolding
  ( Alignment (..)
  , Align(..)
  , alignBoxes
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Figure.Box
import Linear
import Data.List

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

type Label = String

data Scaffolding a = Coupler
                   | Brace (Point2 DisplaySpace) (Scaffolding a)
                   | Joint [(Label,Scaffolding a)]
                   | Transom (Transom a) (Scaffolding a)

data Transom a = Transom1 (   [Label]) (   (Point2 DisplaySpace) -> a)
               | Transom2 (V2 [Label]) (V2 (Point2 DisplaySpace) -> a)
               | Transom3 (V3 [Label]) (V3 (Point2 DisplaySpace) -> a)
               | Transom4 (V4 [Label]) (V4 (Point2 DisplaySpace) -> a)

class Leafable t a => Scaffoldable t a where
  buildScaffolding :: t -> Scaffolding a

class Leafable t a where
  makeLeaf :: t -> a

putGlyph :: (Transformable a, Leafable (Glyph DisplaySpace) a) => Glyph DisplaySpace -> Point2 DisplaySpace -> a
putGlyph glyph p = tTranslate p . makeLeaf $ glyph

instance Leafable (Glyph DisplaySpace) CompoundTree where
  makeLeaf glyph = SLeaf . RawGlyph $ glyph

instance (Transformable a, Leafable (Glyph DisplaySpace) a) => Scaffoldable (Glyph DisplaySpace) a where
  buildScaffolding glyph = Transom (Transom1 ["base"] (putGlyph glyph))
                                   (Joint [("base"  ,                                            Coupler)
                                          ,("width" , Brace (makePoint (glyphAdvanceWidth glyph) 0) Coupler)
                                          ,("height", Brace (makePoint 0 (glyphHeight glyph))       Coupler)]
                                   )

findPoint :: Scaffolding t -> [Label] -> Point2 DisplaySpace
findPoint scaffold [] =
  case scaffold of
    Coupler -> Point2 0 0
    Brace p rest -> p + findPoint rest []
    Transom _ rest -> findPoint rest []
    Joint xs -> error "not enough labels to traverse scaffold"
findPoint scaffold (l:ls) =
  case scaffold of
    Coupler -> error "too many labels to traverse scaffold"
    Brace p rest -> p + findPoint rest (l:ls)
    Transom _ rest -> findPoint rest (l:ls)
    Joint xs -> case find ((==l) . fst) xs of
                  Just (_, found) -> findPoint found ls
                  Nothing -> error $ "label " ++ l ++ "does not match any in joint."

connect :: Scaffolding a -> [Label] -> Scaffolding a -> [Label] -> Scaffolding a
connect s0 ls0 s1 ls1 =
  let p0 = findPoint s0 ls0
      p1 = findPoint s1 ls1
  in  Joint [("left",s0),("right",Brace (p1 - p0) s1)]

stack :: Scaffoldable a t => [a] -> Scaffolding t
stack xs = undefined -- let scaffolds = map buildScaffolding xs
           --in undefined -- foldl1 (connect

rack :: Scaffoldable a t => [a] -> Scaffolding t
rack = undefined -- foldl1 (\next -> Brace (makePoint 1 0)

--paragraph :: DisplaySpace -> DisplaySpace -> String -> CompoundTree
--paragraph = stack . map (rack . map glyph) . lines

newtype Scaffold a = Scaffold {unScaffold :: a}

class Scaffoldable2 a where
  makeScaffold :: a -> Scaffold a

class HasHeight a where
  heightOf :: a -> DisplaySpace

class HasWidth a where
  widthOf :: a -> DisplaySpace

class HasPort a where
  numPorts :: a -> Int
  getPort  :: a -> Point2 DisplaySpace

data MyTree = Branch MyTree MyTree | Leaf Int

instance HasHeight (Scaffold MyTree) where
  heightOf (Scaffold myTree) = undefined

instance HasWidth (Scaffold MyTree) where
  widthOf (Scaffold myTree) = undefined

--stack :: HasHeight a => [a] -> ShapeTree

--class HasEnvelope a where
--  envelope :: a -> Point2 DisplaySpace -> V2 DisplaySpace -> DisplaySpace
--
--class HasMaxTrace a where
--  maxTrace :: a -> Point2 DisplaySpace -> V2 DisplaySpace -> Maybe DisplaySpace
--
--class HasMinTrace a where
--  minTrace :: a -> Point2 DisplaySpace -> V2 DisplaySpace -> Maybe DisplaySpace

--stack :: HasHeight a => [a] -> a
--stack xs = undefined
