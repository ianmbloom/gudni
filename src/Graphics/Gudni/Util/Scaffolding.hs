{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Work in progress, top level datastructure for alignment and text layout.
module Graphics.Gudni.Util.Scaffolding
  ( Alignment (..)
  , MyTree(..)
  , myLeft
  , myRight
  , myLeaf
  , glyphString
  , paragraph
  , distributeRack
  , distributeStack
  , rack
  , stack
  , overlap
  , combineGlyph
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Util.Debug
import Linear
import Data.List
import Data.Char

import Control.Lens
import Control.Monad.State

data Alignment = AlignMin | AlignMax | AlignCenter

alignHorizontal :: (Show (SpaceOf rep), HasSpace rep, Fractional (SpaceOf rep))
                => Alignment
                -> Glyph (STree o rep)
                -> Glyph (STree o rep)
                -> (Glyph (STree o rep), Glyph (STree o rep))
alignHorizontal alignment a b =
  let aWidth = a ^. glyphBox . pX
      bWidth = b ^. glyphBox . pX
      width  = max aWidth bWidth
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - aWidth
                  AlignCenter -> (width - aWidth) / 2
      newA = set (glyphBox . pX) width . mapGlyph (tTranslateXY offsetA 0) $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - bWidth
                  AlignCenter -> (width - bWidth) / 2
      newB = set (glyphBox . pX) width . mapGlyph (tTranslateXY offsetB 0) $ b
  in  (newA, newB)

alignVertical :: (Show (SpaceOf rep), HasSpace rep, Fractional (SpaceOf rep))
              => Alignment
              -> Glyph (STree o rep)
              -> Glyph (STree o rep)
              -> (Glyph (STree o rep), Glyph (STree o rep))
alignVertical alignment a b =
  let aHeight = a ^. glyphBox . pY
      bHeight = b ^. glyphBox . pY
      height  = max aHeight bHeight
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - aHeight
                  AlignCenter -> (height - aHeight) / 2
      newA = set (glyphBox . pY) height . mapGlyph (tTranslateXY 0 offsetA) $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - bHeight
                  AlignCenter -> (height - bHeight) / 2
      newB = set (glyphBox . pY) height . mapGlyph (tTranslateXY 0 offsetB) $ b
  in  (newA, newB)

nextToHorizontal :: (Show (SpaceOf rep), HasSpace rep) => Glyph (STree o rep) -> Glyph (STree o rep) -> (Glyph (STree o rep), Glyph (STree o rep))
nextToHorizontal a b =
    let newB = set (glyphBox . pX) (a ^. glyphBox . pX + b ^. glyphBox . pX) .
               mapGlyph (tTranslateXY (a ^. glyphBox . pX) 0) $
               b
    in (a, newB)

nextToVertical :: (Show (SpaceOf rep), HasSpace rep) => Glyph (STree o rep) -> Glyph (STree o rep) -> (Glyph (STree o rep), Glyph (STree o rep))
nextToVertical a b =
    let newB = set (glyphBox . pY) (a ^. glyphBox . pY + b ^. glyphBox . pY) .
               mapGlyph (tTranslateXY 0 (a ^. glyphBox . pY)) $
               b
    in (a, newB)

combineGlyph :: HasSpace rep
             => (STree o rep -> STree o rep -> STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
combineGlyph op a b =
  set (glyphBox . pX) (max (a ^. glyphBox . pX) (b ^. glyphBox . pX)) .
  set (glyphBox . pY) (max (a ^. glyphBox . pY) (b ^. glyphBox . pY)) .
  set glyphRep (op (a ^. glyphRep) (b ^. glyphRep)) $ a

betweenList :: a -> [a] -> [a]
betweenList b [a]    = [a]
betweenList b (a:as) = a : b : betweenList b as
betweenList b []     = []

distributeRack :: SimpleSpace s => X s -> [Glyph (CompoundTree s)] -> [Glyph (CompoundTree s)]
distributeRack gap = betweenList (Glyph (makePoint gap 0) (SLeaf []))

distributeStack :: SimpleSpace s => Y s -> [Glyph (CompoundTree s)] -> [Glyph (CompoundTree s)]
distributeStack gap = betweenList (Glyph (makePoint 0 gap) (SLeaf []))

overlap :: (HasSpace rep, HasDefault o) => [Glyph (STree o rep)] -> Glyph (STree o rep)
overlap = foldl1 (combineGlyph (SMeld defaultValue))

rack :: forall o rep
     .  (Show rep, Show (SpaceOf rep), Show o,
         HasSpace rep, Fractional (SpaceOf rep), HasDefault o)
     => Alignment
     -> [Glyph (STree o rep)]
     -> Glyph (STree o rep)
rack alignment = foldl1 (\ a b -> uncurry (combineGlyph (SMeld defaultValue)) . uncurry nextToHorizontal $ alignVertical alignment a b)

stack :: forall o rep
      .  (Show rep, Show (SpaceOf rep), Show o,
          HasSpace rep, Fractional (SpaceOf rep), HasDefault o)
      => Alignment
      -> [Glyph (STree o rep)]
      -> Glyph (STree o rep)
stack alignment = foldl1 (\ a b -> uncurry (combineGlyph (SMeld defaultValue)) . uncurry nextToVertical $ alignHorizontal alignment a b)

-- | Convert a string of characters to a list of glyphs in the GlyphMonad.
glyphString :: (MonadState GlyphCache m, Monad m) => String -> m [Glyph (CompoundTree SubSpace)]
glyphString = mapM (getGlyph . CodePoint . ord)

paragraph :: forall m . (MonadState GlyphCache m, Monad m)
          => X SubSpace -> Y SubSpace -> Alignment -> Alignment -> String -> m (Glyph (CompoundTree SubSpace))
paragraph gapX gapY alignX alignY string =
  do  let stringLines = lines string
      glyphLines <- mapM glyphString stringLines
      let glyphRacks = map (rack alignY . distributeRack gapX) glyphLines
      return . stack alignX . distributeStack gapY $ glyphRacks

newtype Scaffold a = Scaffold {unScaffold :: a}

class Scaffoldable2 a where
  makeScaffold :: a -> Scaffold a

class HasPort a where
  numPorts :: a -> Int
  getPort  :: a -> Point2 SubSpace

data MyTree
  = Branch
  { _myLeft  :: MyTree
  , _myRight :: MyTree
  }
  | Leaf
  { _myLeaf :: Int
  } deriving (Show)
makeLenses ''MyTree
