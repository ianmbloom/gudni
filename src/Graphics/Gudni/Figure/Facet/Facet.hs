{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Facet
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structures for applying vector transformations and projections to bitmap textures.

module Graphics.Gudni.Figure.Facet.Facet
  ( Facet_(..)
  , Facet(..)
  , sceneSide
  , textureSide
  , triangleToFacet
  , pairsToFacet
  , facetToBeziers
  , rotateV3
  , rectangleToFacets
  , shouldSubdivideFacet
  )
where

import Graphics.Gudni.Figure.Primitive
--import Graphics.Gudni.Figure.Projection.BezierSpace
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Bezier.Cut
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import qualified Data.Vector as V


import Foreign.C.Types
import Foreign.Storable

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

data Facet_ s = Facet
  { _sceneSide   :: V3 (V2 (Point2 s))
  , _textureSide :: V3 (Point2 s)
  } deriving (Show, Generic)
makeLenses ''Facet_

instance (Out s) => Out (Facet_ s)

type Facet = Facet_ SubSpace

shouldSubdivideBezier :: (Space s) => s -> Bezier s -> Bool
shouldSubdivideBezier tolerance bez =
  let midPoint = mid (bez ^. bzStart) (bez ^. bzEnd)
      tDistance = taxiDistance midPoint (bez ^. bzControl)
  in  tDistance > tolerance

shouldSubdivideFacet :: (Space s) => s -> Facet_ s -> Bool
shouldSubdivideFacet tolerance = or . fmap (shouldSubdivideBezier tolerance) . facetToBeziers

rotateV3 :: V3 a -> V3 a
rotateV3 (V3 a0 a1 a2) = (V3 a1 a2 a0)

facetStartPoints :: Facet_ s -> V3 (Point2 s)
facetStartPoints = fmap (view _x) . view sceneSide

facetControls :: Facet_ s -> V3 (Point2 s)
facetControls  = fmap (view _y) . view sceneSide

facetEndPoints :: Facet_ s -> V3 (Point2 s)
facetEndPoints = fmap (view _x) . rotateV3 . view sceneSide

facetToBeziers :: Facet_ s -> V3 (Bezier s)
facetToBeziers facet =
  let starts   = facetStartPoints facet
      controls = facetControls    facet
      ends     = facetEndPoints   facet
  in  liftA3 Bez starts controls ends


triangleToFacet :: Space s => V3 (Point2 s) -> V3 (Point2 s) -> Facet_ s
triangleToFacet (V3 p0 p1 p2) texture =
  let scene = V3 (V2 p0 (0.5 * (p0 + p1)))
                 (V2 p1 (0.5 * (p1 + p2)))
                 (V2 p2 (0.5 * (p2 + p0)))
  in pairsToFacet scene texture

pairsToFacet :: V3 (V2 (Point2 s)) -> V3 (Point2 s) -> Facet_ s
pairsToFacet scene texture =
  Facet scene texture

rectangleToFacets :: (Space s, Alternative f) => Box s -> f (Facet_ s)
rectangleToFacets box =
      pure (triangleToFacet (V3 (box ^. topLeftBox) (box ^. topRightBox) (box ^. bottomLeftBox))
                            (V3 (box ^. topLeftBox) (box ^. topRightBox) (box ^. bottomLeftBox))
           )
      <|>
      pure (triangleToFacet (V3 (box ^. bottomRightBox) (box ^. bottomLeftBox) (box ^. topRightBox))
                            (V3 (box ^. bottomRightBox) (box ^. bottomLeftBox) (box ^. topRightBox))
           )

instance (Space s) => HasSpace (Facet_ s) where
    type SpaceOf (Facet_ s) = s

boxOfPointsV2 :: Ord s => V2 (Point2 s) -> Box s
boxOfPointsV2 (V2 a b) = boxAroundPoints a b

instance (Space s) => CanBox (Facet_ s) where
    boxOf (Facet scene texture) = foldl1 minMaxBox . fmap boxOfPointsV2 $ scene

instance (NFData s) => NFData (Facet_ s) where
  rnf (Facet scene texture) = scene `deepseq` texture `deepseq` ()

instance StorableM Facet where
    sizeOfM _ = do sizeOfM (undefined :: V3 (V2 (Point2 SubSpace )))
                   sizeOfM (undefined :: V3 (Point2 TextureSpace))
    alignmentM _ = do alignmentM (undefined :: V3 (V2 (Point2 SubSpace)))
                      alignmentM (undefined :: V3 (Point2 TextureSpace) )
    peekM = do scene   <- peekM
               texture <- peekM
               return $ Facet scene texture
    pokeM (Facet scene texture) = do pokeM scene
                                     pokeM texture

instance Storable Facet where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
