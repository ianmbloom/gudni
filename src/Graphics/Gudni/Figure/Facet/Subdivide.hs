{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Subdivide
  ( subdivideFacetSteps
  , tesselateFacet
  , subdivideFacet
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Facet.Triangle
import Graphics.Gudni.Figure.Facet.BezierTriangle
import Graphics.Gudni.Figure.Facet.Type

import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Foldable
import qualified Data.Vector as V

import Text.PrettyPrint.GenericPretty
import Control.Lens

instance (Out a) => Out (V4 a)

subdivideFacetSteps :: forall s
                    .  (Space s)
                    => Int
                    -> Facet_ s
                    -> [Facet_ s]
subdivideFacetSteps steps =
  go steps
  where
  go :: Int -> Facet_ s -> [Facet_ s]
  go steps facet =
     if steps > 0
     then join . fmap (go (steps - 1)) . toList . subdivideFacet $ facet
     else pure facet

tesselateFacet :: forall s
               .  (Space s)
               => s
               -> Facet_ s
               -> [Facet_ s]
tesselateFacet tolerance =
  go
  where
  go :: Facet_ s -> [Facet_ s]
  go facet =
      let subFacets = subdivideFacetSteps 1 facet
          (continued, done) = segregate (shouldSubdivideFacet tolerance) subFacets
      in  done <|> if null continued then []  else join . fmap go $ continued

subdivideFacet :: (Space s) => Facet_ s -> V4 (Facet_ s)
subdivideFacet facet =
  let output = facet ^. facetOutput
      sideOuts = sideBezTris output
      centerOut = centerBezTri sideOuts

      input = facet ^. facetInput
      sideIns = sideTris input
      centerIn = centerTri input

  in  V4 (Facet (sideOuts ^. _x) (sideIns ^. _x))
         (Facet (sideOuts ^. _y) (sideIns ^. _y))
         (Facet (sideOuts ^. _z) (sideIns ^. _z))
         (Facet  centerOut       centerIn     )
