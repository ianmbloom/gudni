{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Fit.Facet
  (
  )
where

import Graphics.Gudni.Base.Reversible
import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Base.Loop

import Graphics.Gudni.Figure.Primitive

import Graphics.Gudni.Figure.Transform.Projection
import Graphics.Gudni.Figure.Fit.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.Bezier.Split
import Graphics.Gudni.Figure.Bezier.Deknob

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Maybe (fromMaybe, fromJust)
import Text.PrettyPrint.GenericPretty

mapOverSceneBez :: Space s => (Bezier s -> Bezier s) -> Facet_ s -> Facet_ s
mapOverSceneBez f = undefined -- over facetSides (fmap (over facetOutput f))

instance Reversible (Facet_ s) where
    reverseItem = id

instance Space s => CanFit (Facet_ s) where
    isForward facet = True -- the order of facets doesn't matter.
    projectTangent offset v0 normal = mapOverSceneBez (projectTangent offset v0 normal)
    fillGap leftResult rightResult = leftResult <|> rightResult
    projectDefaultCurve debugFlag max_steps m_accuracy start sourceCurve =
         mapOverSceneBez (projectDefaultCurve debugFlag max_steps m_accuracy start sourceCurve)
