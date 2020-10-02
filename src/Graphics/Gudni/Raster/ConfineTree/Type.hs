{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.Gudni.Raster.ConfineTree.Type
  ( ItemStack(..)
  , ConfineTree (..)
  , Confine(..)
  , Branch(..)
  , confineCurve
  , confineCut
  , confineOverhang
  , confineLessCut
  , confineMoreCut
  , DecoTree(..)
  , DecorateTree(..)
  , decoCurveTag
  , decoCut
  , decoCrossings
  , decoLessCut
  , decoMoreCut
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier

import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad

import Data.Kind

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

type ItemStack = [ItemTagId]

data Confine axis s
    = Confine
    { _confineCurve     :: TaggedBezier s
    , _confineCut       :: Athwart axis s
    , _confineOverhang  :: Athwart axis s
    , _confineLessCut   :: Maybe (Confine (PerpendicularTo axis) s)
    , _confineMoreCut   :: Maybe (Confine (PerpendicularTo axis) s)
    }
    deriving (Generic)
makeLenses ''Confine


deriving instance (Axis axis, Show s) => Show (Confine axis s)

type Branch axis s = Maybe (Confine axis s)
type ConfineTree s = Branch Vertical s

instance (Space s, Axis axis, Out s, Out axis, Out (PerpendicularTo axis), axis~PerpendicularTo(PerpendicularTo axis)) => Out (Confine axis s)
   where
    doc tree =
        ( text (show (tree ^. confineCurve)) <+>
          text "--" <+>
          doc (tree ^. confineCut) <+>
          doc (tree ^. confineOverhang)
         )
         $$
         nest 4 ( outMConfineTree (tree ^. confineLessCut) $$ outMConfineTree (tree ^. confineMoreCut))
    docPrec _ = doc

outMConfineTree mTree =
        case mTree of
          Nothing -> text "X"
          Just tree -> doc tree

data DecoTree axis s
    = DecoBranch
      { _decoCurveTag  :: CurveTag
      , _decoCut       :: Athwart axis s
      , _decoCrossings :: ItemStack
      , _decoLessCut   :: DecoTree (PerpendicularTo axis) s
      , _decoMoreCut   :: DecoTree (PerpendicularTo axis) s
      }
    | DecoLeaf
makeLenses ''DecoTree

type DecorateTree s = DecoTree Vertical s

instance ( Space s
         , Axis axis
         , Out s
         , Out axis
         , Out (PerpendicularTo axis)
         , axis~PerpendicularTo(PerpendicularTo axis))
         => Out (DecoTree axis s)
   where
    doc tree =
        case tree of
            DecoBranch {} ->
                ( doc (tree ^?! decoCurveTag) <+>
                  doc (tree ^?! decoCut     ) <+>
                  doc (tree ^?! decoCrossings)
                )
                $$
                nest 4 ( doc (tree ^?! decoLessCut) $$
                         doc (tree ^?! decoMoreCut)
                       )
            DecoLeaf {} ->
                text "X"
    docPrec _ = doc