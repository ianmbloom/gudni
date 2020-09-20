{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Traverse
  ( traverseFacet
  , traversePotentialFacet
  )
where

import Graphics.Gudni.Base.Chain

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Facet.Facet
import Graphics.Gudni.Figure.Facet.Subdivide

import Data.Foldable
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative
import Text.PrettyPrint.GenericPretty

insideBox :: Space s
          => Point2 s
          -> Box s
          -> Bool
insideBox p box =
       box ^. leftSide   <= p ^. pX
    && box ^. topSide    <= p ^. pY
    && box ^. rightSide  >  p ^. pX
    && box ^. bottomSide >  p ^. pY

couldContain :: Space s => Point2 s -> Facet_ s -> Bool
couldContain p facet = insideBox p . boxOf $ facet

limit :: Space s => s
limit = 1 / 16

sizeLimit :: Space s => Facet_ s -> Bool
sizeLimit facet =
  let box = boxOf facet
  in (box ^. heightBox > limit) || (box ^. widthBox > limit)

insideFacet :: Space s => Point2 s -> Facet_ s -> Bool
insideFacet point =
  foldl1 (/=) .
  fmap (crossesAlong Vertical (point ^. pX) minBound (point ^. pY)) .
  facetToBeziers

traversePotentialFacet :: forall s . (Space s) => s -> Point2 s -> Facet_ s -> [Facet_ s]
traversePotentialFacet threshold point =
  go
  where
  go :: Facet_ s -> [Facet_ s]
  go facet =
    let potential = filter (insideFacet point) . subdivideFacetSteps 1 $ facet
        (rest, done) = segregate (shouldSubdivideFacet threshold) potential
    in  done <|> if null rest then [] else concatMap go rest


traverseFacet :: (Space s) => s -> Point2 s -> Facet_ s -> [Facet_ s]
traverseFacet threshold point =
  traversePotentialFacet threshold point
