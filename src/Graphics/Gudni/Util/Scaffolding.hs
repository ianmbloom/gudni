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

data TextState s = TextState
  { _textAdvance   :: X s
  , _textRetreat   :: X s
  , _textAscent    :: Y s
  , _textDescent   :: Y s
  }
makeLenses ''TextState

emptyTextState :: (Num s) => TextState s
emptyTextState = TextState
  { _textAdvance = 0
  , _textRetreat = 0
  , _textAscent  = 0
  , _textDescent = 0
  }

type TextMonad s = State (TextState s)

rack :: forall o t rep
     .  (HasSpace rep, Ord (SpaceOf rep), Num (SpaceOf rep), HasDefault o)
     => [Glyph (STree o rep)]
     -> Glyph (STree o rep)
rack row =
  let (unalignedRow, state) = runState (go row) emptyTextState
      go :: [Glyph (STree o rep)] -> TextMonad (SpaceOf rep) [X (SpaceOf rep)]
      go (c:cs) =
          do  currentRetreat <- use textRetreat
              currentAdvance <- use textAdvance
              textRetreat .= currentRetreat + currentAdvance + c ^. glyphRetreat
              textAdvance .= c ^. glyphAdvance
              textAscent  %= max (c ^. glyphAscent )
              textDescent %= max (c ^. glyphDescent)
              (currentRetreat:) <$> go cs
      go [] = return []
      align ascent c pos = mapGlyph (tTranslate (makePoint pos (ascent - c ^. glyphAscent))) c
  in  overlap $ zipWith (align (state ^. textAscent)) row unalignedRow

stack :: forall o t rep
      .  (HasSpace rep, Ord (SpaceOf rep), Num (SpaceOf rep), HasDefault o)
      => [Glyph (STree o rep)]
      -> Glyph (STree o rep)
stack column =
  let (unalignedColumn, state) = runState (go column) emptyTextState
      go :: [Glyph (STree o rep)] -> TextMonad (SpaceOf rep) [Y (SpaceOf rep)]
      go (l:ls) =
          do currentAscent  <- use textAscent
             currentDescent <- use textDescent
             textAscent  .= currentAscent + currentDescent + (l ^. glyphAscent)
             textDescent .= l ^. glyphDescent
             textAdvance %= max (l ^. glyphAdvance)
             textRetreat %= max (l ^. glyphRetreat)
             (currentAscent:) <$> go ls
      go [] = return []
      align retreat c pos = mapGlyph (tTranslate (makePoint (retreat - c ^. glyphRetreat) pos)) c
  in  overlap $ zipWith (align (state ^. textRetreat)) column unalignedColumn

betweenList :: (Glyph a -> Glyph a) -> [Glyph a] -> [Glyph a]
betweenList f (a:as) = a : map f as
betweenList f [] = []

distributeRack :: HasSpace a => X (SpaceOf a) -> [Glyph a] -> [Glyph a]
distributeRack gap = betweenList (over glyphRetreat (+gap))

distributeStack :: HasSpace a => Y (SpaceOf a) -> [Glyph a] -> [Glyph a]
distributeStack gap = betweenList (over glyphAscent (+gap))

-- | Convert a string of characters to a list of glyphs in the GlyphMonad.
glyphString :: (MonadState GlyphCache m, Monad m) => String -> m [Glyph (CompoundTree SubSpace)]
glyphString = mapM (fmap (mapGlyph SLeaf) . getGlyph . CodePoint . ord)

paragraph :: forall m . Monad m => X SubSpace -> Y SubSpace -> Alignment -> String -> GlyphMonad m (Glyph (CompoundTree SubSpace))
paragraph gapX gapY alignment string =
  do  let stringLines = lines string
      glyphLines <- mapM glyphString stringLines
      let glyphRacks = map (realignHorizontal alignment . rack . distributeRack gapX) glyphLines
      return . stack . distributeStack gapY $ glyphRacks

data Alignment = AlignMin | AlignMax | AlignCenter

realignHorizontal :: Fractional (SpaceOf a) => Alignment -> Glyph a -> Glyph a
realignHorizontal alignment g =
  let width = g ^. glyphAdvance + g ^. glyphRetreat
  in case alignment of
    AlignMin    -> set glyphRetreat 0 . set glyphAdvance width $ g
    AlignMax    -> set glyphRetreat width . set glyphAdvance 0 $ g
    AlignCenter -> set glyphAdvance (width / 2) . set glyphRetreat (width / 2) $ g

realignVertical :: Fractional (SpaceOf a) => Alignment -> Glyph a -> Glyph a
realignVertical alignment g =
  let height = g ^. glyphAscent + g ^. glyphDescent
  in case alignment of
    AlignMin    -> set glyphAscent 0 . set glyphDescent height $ g
    AlignMax    -> set glyphAscent height . set glyphDescent 0 $ g
    AlignCenter -> set glyphAscent (height / 2) . set glyphDescent (height / 2) $ g

combineGlyph :: Ord (SpaceOf a) => (a -> a -> a) -> Glyph a -> Glyph a -> Glyph a
combineGlyph op (Glyph advance0 retreat0 ascent0 descent0 a0)
                (Glyph advance1 retreat1 ascent1 descent1 a1) =
                  Glyph (max advance0 advance1)
                        (max retreat0 retreat1)
                        (max ascent0  ascent1 )
                        (max descent0 descent1)
                        (op a0 a1)

overlap :: (Ord (SpaceOf rep), HasDefault o) => [Glyph (STree o rep)] -> Glyph (STree o rep)
overlap = foldl1 (combineGlyph (SMeld defaultValue))


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
