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

instance Space s => CanFit (Facet_ s) where
    isForward facet = undefined -- (Bez v0 _ v1) = v0 ^. pX <= v1 ^. pX
    reverseItem = mapOverSceneBez reverseBezier
    projectTangent = undefined -- projectTangentBezier
    fillGap leftResult rightResult =
        undefined
        {-
        let leftEnd  = (lastLink leftResult  ) ^. bzEnd
            rightStart = (firstLink rightResult) ^. bzStart
            filler = if leftEnd /= rightStart
                     then pure (line leftEnd rightStart)
                     else empty
        in leftResult <|> filler <|> rightResult
        -}
    projectOntoCurve debugFlag max_steps m_accuracy start sourceCurve =
         mapOverSceneBez (projectOntoCurve debugFlag max_steps m_accuracy start sourceCurve)

instance (s ~ (SpaceOf (f (Facet_ s)))
         , Space s
         --, Show (f (Bezier s))
         --, Show (f (Facet_ s))
         , Chain f
         --, CanCut (Facet_ s)
         ) => CanProject (BezierSpace s) (f (Facet_ s)) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace facets =
        let fixed :: f (Facet_ s)
            fixed = join . fmap replaceKnob $ facets
        in  join . fmap (traverseBezierSpace debug max_steps m_accuracy bSpace) $ fixed
