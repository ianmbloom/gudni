{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.FitFacet
  (
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Figure.FitBezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Reversible
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
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

mapOverSceneBez :: Space s => (Bezier s -> Bezier s) -> Facet_ s -> Facet_ s
mapOverSceneBez f = over facetSides (fmap (over sceneSide f))

instance Reversible (Facet_ s) where
    reverseItem = id

instance Space s => CanFit (Facet_ s) where
    isForward facet = True -- the order of facets doesn't matter.
    projectTangent offset v0 normal = mapOverSceneBez (projectTangent offset v0 normal)
    fillGap leftResult rightResult = leftResult <|> rightResult
    projectOntoCurve debugFlag max_steps m_accuracy start sourceCurve =
         mapOverSceneBez (projectOntoCurve debugFlag max_steps m_accuracy start sourceCurve)

{-
instance ( Space s
         , Chain f

         ) => CanProject (BezierSpace s) (Facet_ s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace facets =
        let fixed :: f (Facet_ s)
            fixed = join . fmap (replaceKnob verticalAxis) $ facets
        in  join . fmap (traverseBezierSpace debug max_steps m_accuracy bSpace) $ fixed
-}
