{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

module Graphics.Gudni.Figure.Facet
  (
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Box

import Linear.V3
import Linear.V2
import Control.Lens
import Control.Applicative

import Foreign.C.Types
import Foreign.Storable
import Graphics.Gudni.Util.StorableM

data TextureThreshold s t = TexThreshold
  { _sceneSide   :: Bezier s
  , _textureSide :: V2 (Point2 t)
  }
makeLenses ''TextureThreshold

data Facet_ s t = Facet
  { _facetSides :: V3 (TextureThreshold s t)
  }
makeLenses ''Facet_

type Facet = Facet_ SubSpace TextureSpace

data HardFacet_ s t = HardFacet
  { _hardFace :: V3 (Point2 s)
  , _hardTexture :: V3 (Point2 t)
  }
makeLenses ''HardFacet_

type HardFacet = HardFacet_ SubSpace TextureSpace

taxiDistance :: (Space s) => Point2 s -> Point2 s -> s
taxiDistance v0 v1 =
  abs(unOrtho $ v1 ^. pX - v0 ^. pX) + abs(unOrtho $ v1 ^. pY - v0 ^. pY)

shouldSubdivideBezier :: (Space s) => s -> Bezier s -> Bool
shouldSubdivideBezier tolerance bez =
  let midPoint = mid (bez ^. bzStart) (bez ^. bzEnd)
      tDistance = taxiDistance midPoint (bez ^. bzControl)
  in  tDistance > tolerance

shouldSubdivideFacet :: (Space s, Space t) => s -> Facet_ s t -> Bool
shouldSubdivideFacet tolerance = or . fmap (shouldSubdivideBezier tolerance . view sceneSide) . view facetSides

--             *            --
--            / \           --
--           o   o          --
--          /     \         --
--         * - o - *        --
--       /  \     / \       --
--      o    o   o   o      --
--     /      \ /     \     --
--    * - o - * - o - *     --

splitLineSegment :: (Space t) => t -> V2 (Point2 t) -> (V2 (Point2 t), V2 (Point2 t))
splitLineSegment t (V2 v0 v1) = let midPoint = lerp t v0 v1 in (V2 v0 midPoint, V2 midPoint v1)

splitBezierSide :: (Space s, Space t) => TextureThreshold s t -> (TextureThreshold s t, TextureThreshold s t)
splitBezierSide (TexThreshold bez tex) =
  let (leftBez, rightBez) = splitBezier 0.5 bez
      (leftTex, rightTex) = splitLineSegment 0.5 tex
  in  (TexThreshold leftBez leftTex, TexThreshold rightBez rightTex)

sideTriangle :: (Space s, Space t)
             => Lens' (V3 (TextureThreshold s t)) (TextureThreshold s t)
             -> Lens' (V3 (TextureThreshold s t)) (TextureThreshold s t)
             -> Lens' (V3 (TextureThreshold s t)) (TextureThreshold s t)
             -> Facet_ s t
             -> Facet_ s t
sideTriangle left right bottom facet =
  let (_, left')  = splitBezierSide (facet ^. facetSides . left)
      (right', _) = splitBezierSide (facet ^. facetSides . right)
      midControl = mid (facet ^. facetSides . left . sceneSide . bzEnd) (facet ^. facetSides . bottom . sceneSide . bzControl)
      bottomBez = Bez (left' ^. sceneSide . bzStart) midControl (right' ^. sceneSide . bzEnd)
      bottomTex = V2 (left' ^. textureSide . _x) (right' ^. textureSide . _y)
      bottom' = TexThreshold bottomBez bottomTex
  in  set (facetSides . left) left' . set (facetSides . right) right' . set (facetSides . bottom) bottom' $ facet

subdivideFacet :: (Space s, Space t, Alternative f) => Facet_ s t -> f (Facet_ s t)
subdivideFacet facet =
  let triZ  = sideTriangle _x _y _z facet
      triX  = sideTriangle _y _z _x facet
      triY  = sideTriangle _z _x _y facet
      triIn = Facet {_facetSides = V3 (triZ ^. facetSides . _z)
                                      (triX ^. facetSides . _x)
                                      (triY ^. facetSides . _y)
                    }
  in  pure triZ <|> pure triX <|> pure triY <|> pure triIn

stripBezier :: Bezier s -> V2 (Point2 s)
stripBezier (Bez v0 c v1) = V2 v0 v1

hardenFacet :: Facet_ s t -> HardFacet_ s t
hardenFacet facet =
    let sceneFacet = fmap (view _x . stripBezier . view sceneSide) . view facetSides $ facet
        textureFacet = fmap (view (textureSide . _x)) . view facetSides $ facet
    in  HardFacet { _hardFace = sceneFacet, _hardTexture = textureFacet}

instance (Space s) => HasSpace (Facet_ s t) where
  type SpaceOf (Facet_ s t) = s

instance (Space s) => HasBox (Facet_ s t) where
    boxOf (Facet v3) = foldl1 minMaxBox (fmap (boxOf . view sceneSide) v3)

instance StorableM HardFacet where
    sizeOfM _ = do sizeOfM (undefined :: V3 (Point2 SubSpace))
                   sizeOfM (undefined :: V3 (Point2 TextureSpace))
    alignmentM _ = do alignmentM (undefined :: V3 (Point2 SubSpace))
                      alignmentM (undefined :: V3 (Point2 TextureSpace))
    peekM = do scene   <- peekM
               texture <- peekM
               return (HardFacet scene texture)
    pokeM (HardFacet scene texture) = do pokeM scene
                                         pokeM texture

instance Storable HardFacet where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
