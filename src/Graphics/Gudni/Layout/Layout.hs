{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Adjacent
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Layout
  ( Alignment(..)
  , Adjacency(..)
  , AdjacentMeld(..)
  , Layout(..)
  , LayoutRep(..)
  , LayoutCompound(..)
  , LayoutCompoundRep(..)
  , place
  , placeMask
  , mapLayoutFormSubstance
  , mapLayoutFormToken
  , fromLayout
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Layout.Fill
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Overlappable
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.MaybeBox
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Linear
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Control.Applicative

data Adjacency
    = NextTo
    { _adjNextTo :: EitherAxis
    , _adjAlign  :: Maybe Alignment
    }
    | OnTopOf
    { _adjOverlapAlignVertical   :: Maybe Alignment
    , _adjOverlapAlignHorizontal :: Maybe Alignment
    }
    deriving (Show)
makeLenses ''Adjacency

instance HasDefault Adjacency where
  defaultValue = OnTopOf Nothing Nothing

data AdjacentMeld style meld
    = AdjacentMeld
    { _layAdjStyle :: style
    , _layAdjType  :: Adjacency
    , _layAdjMeld  :: meld
    } deriving (Show)
makeLenses ''AdjacentMeld

instance (HasDefault style, HasDefault meld) => HasDefault (AdjacentMeld style meld) where
    defaultValue = AdjacentMeld defaultValue defaultValue defaultValue

data LayoutCompoundRep s style
  = Glyph
  { _layGlyphStyle :: style
  , _layGlyph      :: CodePoint
  }
  | CompoundItem
  { _layCompoundItem :: CompoundTree s
  , _layCompoundItemBox :: Maybe (Box s)
  } deriving (Show)
makeLenses ''LayoutCompoundRep

type LayoutCompound s style = STree (AdjacentMeld style Compound) (Maybe (LayoutCompoundRep s style))

data LayoutRep token s style
  = Item
  { _layItem :: ShapeTree token s
  , _layItemBox :: Maybe (Box s)
  }
  | Form
  { _layFormSubstance :: Substance NamedTexture s
  , _layFormToken :: Maybe token
  , _layForm :: LayoutCompound s style
  }
makeLenses ''LayoutRep

type Layout token s style = STree (AdjacentMeld style Overlap) (Maybe (LayoutRep token s style ))

deriving instance (Space s, Show style, Show token) => Show (LayoutRep token s style)

instance (Space s) => HasSpace (LayoutRep token s style) where
  type SpaceOf (LayoutRep token s style) = s

instance (Space s) => HasSpace (LayoutCompoundRep s style) where
  type SpaceOf (LayoutCompoundRep s style) = s

instance (Space s) => CanFill (Layout token s style) where
  type UnFilled (Layout token s style) = LayoutCompound s style
  withFill substance = SLeaf . Just . Form substance Nothing

place :: (IsStyle style) => ShapeTree token s -> Layout token s style
place item = SLeaf . Just . Item item $ Nothing

placeMask :: (IsStyle style) => CompoundTree s -> LayoutCompound s style
placeMask item = SLeaf . Just . CompoundItem item $ Nothing

mapLayoutFormSubstance :: (Substance NamedTexture s -> Substance NamedTexture s) -> Layout token s style -> Layout token s style
mapLayoutFormSubstance f =
  mapSTree . fmap $ \rep ->
     case rep of
       Form substance token child -> Form (f substance) token child
       _ -> rep

mapLayoutFormToken :: (Maybe token -> Maybe token) -> Layout token s style -> Layout token s style
mapLayoutFormToken f =
  mapSTree . fmap $ \rep ->
     case rep of
       Form substance token child -> Form substance (f token) child
       _ -> rep

keep :: a -> b -> (b, b)
keep _ b = (b, b)

const2 :: (c -> d) -> a -> b -> c -> d
const2 f a b c = f c

overMaybe :: Monad m => (a -> FontMonad m b) -> Maybe a -> FontMonad m (Maybe b)
overMaybe f mA =
  case mA of
    Nothing -> return Nothing
    Just a -> Just <$> f a

overMaybe2 :: Monad m => (a -> FontMonad m b) -> c -> d -> Maybe a -> FontMonad m (Maybe b)
overMaybe2 = const2 . overMaybe


fromLayout :: forall token s style m .
              ( s~SpaceOf style
              , IsStyle style
              , Monad m
              , Show token
              )
           => Layout token s style
           -> FontMonad m (Maybe (ShapeTree token s))
fromLayout layout =
  let traversed :: FontMonad m (Maybe (MaybeBox (ShapeTree token s)))
      traversed =  traverseTree keep
                                (\trans -> fmap (over maybeItem (STransform trans)))
                                const
                                defaultValue
                                IdentityTransform
                                (const2 onLayoutLeaf)
                                (eitherMaybeMeld onLayoutMeld)
                                layout
  in  fmap (view maybeItem) <$> traversed

eitherMaybeMeld :: (t -> a -> a -> a) -> t -> Maybe a -> Maybe a -> Maybe a
eitherMaybeMeld f a = eitherMaybe (f a)

fromCompoundLayout :: ( s~SpaceOf style
                      , IsStyle style
                      , Monad m
                      , Space s
                      )
                   => Maybe (LayoutCompound s style)
                   -> FontMonad m (Maybe (MaybeBox (CompoundTree s)))
fromCompoundLayout mLayout =
                     let doLeaf :: AdjacentMeld style Compound
                                -> Transformer s
                                -> Maybe (LayoutCompoundRep s style)
                                -> FontMonad m (Maybe (MaybeBox (CompoundTree s)))
                         doLeaf = overMaybe2 onLayoutCompoundLeaf
                     in
                     _ (traverseTree keep
                                     (\trans -> over maybeItem (STransform trans))
                                     (const id)
                                     defaultValue
                                     IdentityTransform
                                     doLeaf
                                     (eitherMaybeMeld onLayoutMeld))
                                     mLayout

onLayoutLeaf :: forall token s style m
             .  ( s~SpaceOf style
                , IsStyle style
                , Monad m
                )
             => Maybe (LayoutRep token s style)
             -> FontMonad m (Maybe (MaybeBox (ShapeTree token s)))
onLayoutLeaf mRep =
    case mRep of
       Nothing -> return Nothing
       Just rep ->
            case rep of
              Item item mBox -> return $ Just $ MaybeBox item mBox
              Form substance mToken layout ->
                  do (mCompound :: Maybe (MaybeBox (CompoundTree s))) <- fromCompoundLayout $ Just layout
                     overMaybe (\ (MaybeBox child mBox) ->
                                       let shapeTree :: ShapeTree token s
                                           shapeTree = overToken (const mToken) . withFill substance $ child
                                       in  return $ MaybeBox shapeTree mBox
                               ) mCompound

onLayoutCompoundLeaf :: ( s~SpaceOf style
                        , IsStyle style
                        , Monad m
                        )
                     => LayoutCompoundRep s style
                     -> FontMonad m (MaybeBox (CompoundTree s))
onLayoutCompoundLeaf rep =
    case rep of
        Glyph style codePoint  -> do (shape, box) <- styleGlyph style codePoint
                                     return $ MaybeBox (SLeaf $ Just shape) box
        CompoundItem item mBox -> return $ MaybeBox item mBox

onLayoutMeld :: ( SpaceOf style~SpaceOf leaf
                , IsStyle style
                , HasBox leaf
                , CanProject (BezierSpace (SpaceOf leaf)) leaf
                , Transformable leaf
                )
             => AdjacentMeld style meld
             -> MaybeBox (STree meld (Maybe leaf))
             -> MaybeBox (STree meld (Maybe leaf))
             -> MaybeBox (STree meld (Maybe leaf))
onLayoutMeld (AdjacentMeld style adjacency meld) a b =
    withBoxToMaybeBox $
    meldWithBox meld $
    let aC = collapseMaybeBox a
        bC = collapseMaybeBox b
    in
    case adjacency of
        NextTo eAxis mAlignment ->
           let (a0, b0) = (fromEitherAxis (nextTo Horizontal) (nextTo Vertical) eAxis) aC bC
               (a1, b1) = case mAlignment of
                            Nothing -> (a0, b0)
                            Just alignment -> (fromEitherAxis (align Horizontal) (align Vertical) eAxis) alignment a0 b0
           in (a1, b1)
        OnTopOf mAlignHori mAlignVert ->
           let (a0, b0) = case mAlignHori of
                             Nothing -> (aC,bC)
                             Just alignHori -> align Horizontal alignHori aC bC
               (a1, b1) = case mAlignVert of
                             Nothing -> (a0,b0)
                             Just alignVert -> align Vertical alignVert a0 b0
            in (a1, b1)

whenBothNotEmpty :: HasEmpty a => a -> a -> (a, a) -> (a, a)
whenBothNotEmpty a b f =
  if isEmpty a || isEmpty b
  then (a, b)
  else f

alignWithSize :: (Axis axis, SimpleTransformable t)
              => axis
              -> Alignment
              -> WithBox t
              -> SpaceOf t
              -> WithBox t
alignWithSize axis alignment a size =
    let aSize = a ^. withBox . acrossBox axis
        offsetA = case alignment of
                    AlignMin    -> 0
                    AlignMax    -> size - aSize
                    AlignCenter -> (size - aSize) / 2
        newA = translateOnAxis (nextAxis axis) offsetA $ a
    in  newA


align :: ( HasBox leaf
         , CanProject (BezierSpace (SpaceOf leaf)) leaf
         , Transformable leaf
         , Axis axis)
      => axis
      -> Alignment
      -> WithBox (STree meld leaf)
      -> WithBox (STree meld leaf)
      -> (WithBox (STree meld leaf)
         ,WithBox (STree meld leaf)
         )
align axis alignment a b =
  let aSize = a ^. withBox . acrossBox axis
      bSize = b ^. withBox . acrossBox axis
      size  = max aSize bSize
      a' = alignWithSize axis alignment a size
      b' = alignWithSize axis alignment b size
  in  (a', b')

nextTo :: ( HasBox leaf
          , CanProject (BezierSpace (SpaceOf leaf)) leaf
          , Transformable leaf
          , Axis axis)
       => axis
       ->  WithBox (STree meld leaf)
       ->  WithBox (STree meld leaf)
       -> (WithBox (STree meld leaf)
          ,WithBox (STree meld leaf)
          )
nextTo axis a b =
  let d = a ^. withBox . maxBox . along axis - b ^. withBox . minBox . along axis
      newB = translateOnAxis (nextAxis axis) d b
  in  (a, newB)
