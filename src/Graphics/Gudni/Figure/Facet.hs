{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  ( Facet_(..)
  , HardFacet_(..)
  , hardenFacet
  , tesselateFacet
  , rectangleToFacets
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Util.Chain

import Linear.V3
import Linear.V2
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.DeepSeq


import Foreign.C.Types
import Foreign.Storable
import Graphics.Gudni.Util.StorableM

data FacetSide s t = FacetSide
  { _sceneSide   :: Bezier s
  , _textureSide :: V2 (Point2 t)
  } deriving (Show)
makeLenses ''FacetSide

data Facet_ s t = Facet
  { _facetSides :: V3 (FacetSide s t)
  } deriving (Show)
makeLenses ''Facet_

type Facet = Facet_ SubSpace TextureSpace

data HardFacet_ s t = HardFacet
  { _hardFace :: V3 (Point2 s)
  , _hardTexture :: V3 (Point2 t)
  } deriving (Show)
makeLenses ''HardFacet_

type HardFacet = HardFacet_ SubSpace TextureSpace

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

splitBezierSide :: (Space s, Space t) => FacetSide s t -> (FacetSide s t, FacetSide s t)
splitBezierSide (FacetSide bez tex) =
  let (leftBez, rightBez) = splitBezier 0.5 bez
      (leftTex, rightTex) = splitLineSegment 0.5 tex
  in  (FacetSide leftBez leftTex, FacetSide rightBez rightTex)

sideTriangle :: (Space s, Space t)
             => Lens' (V3 (FacetSide s t)) (FacetSide s t)
             -> Lens' (V3 (FacetSide s t)) (FacetSide s t)
             -> Lens' (V3 (FacetSide s t)) (FacetSide s t)
             -> Facet_ s t
             -> Facet_ s t
sideTriangle left right bottom facet =
  let (_, left')  = splitBezierSide (facet ^. facetSides . left)
      (right', _) = splitBezierSide (facet ^. facetSides . right)
      midControl = mid (facet ^. facetSides . left . sceneSide . bzEnd) (facet ^. facetSides . bottom . sceneSide . bzControl)
      bottomBez = Bez (left' ^. sceneSide . bzStart) midControl (right' ^. sceneSide . bzEnd)
      bottomTex = V2 (left' ^. textureSide . _x) (right' ^. textureSide . _y)
      bottom' = FacetSide bottomBez bottomTex
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

tesselateFacet :: (Space s, Space t, Chain f)
               => SpaceOf (Facet_ s t) -> Facet_ s t -> f (Facet_ s t)
tesselateFacet tolerance facet =
  let subFacets = subdivideFacet facet
      (continued, done) = segregate (shouldSubdivideFacet tolerance) subFacets
      rest = join . fmap (tesselateFacet tolerance) $ continued
  in  (rest <|> done)

stripBezier :: Bezier s -> V2 (Point2 s)
stripBezier (Bez v0 c v1) = V2 v0 v1

hardenFacet :: Facet_ s t -> HardFacet_ s t
hardenFacet facet =
    let sceneFacet = fmap (view _x . stripBezier . view sceneSide) . view facetSides $ facet
        textureFacet = fmap (view (textureSide . _x)) . view facetSides $ facet
    in  HardFacet { _hardFace = sceneFacet, _hardTexture = textureFacet}

linesToFacetSide :: Space s => Point2 s -> Point2 s -> Point2 t -> Point2 t -> FacetSide s t
linesToFacetSide p0 p1 t0 t1 = FacetSide (straight p0 p1) (V2 t0 t1)

triangleToFacet :: Space s => V3 (Point2 s) -> V3 (Point2 t) -> Facet_ s t
triangleToFacet (V3 p0 p1 p2) (V3 t0 t1 t2) =
  Facet $
  V3 (linesToFacetSide p0 p1 t0 t1)
     (linesToFacetSide p1 p2 t1 t2)
     (linesToFacetSide p2 p0 t2 t0)

rectangleToFacets :: (Space s, Space t, Alternative f) => (t -> s) -> Point2 t -> f (Facet_ s t)
rectangleToFacets convert size =
  let sBox = pointToBox (fmap convert size)
      tBox = pointToBox size
  in  pure (triangleToFacet (V3 (sBox ^. topLeftBox) (sBox ^. topRightBox) (sBox ^. bottomLeftBox))
                            (V3 (tBox ^. topLeftBox) (tBox ^. topRightBox) (tBox ^. bottomLeftBox))
           )
      <|>
      pure (triangleToFacet (V3 (sBox ^. bottomRightBox) (sBox ^. bottomLeftBox) (sBox ^. topRightBox))
                            (V3 (tBox ^. bottomRightBox) (tBox ^. bottomLeftBox) (tBox ^. topRightBox))
           )

instance (Space s) => HasSpace (FacetSide s t) where
    type SpaceOf (FacetSide s t) = s

instance (Space s) => HasSpace (Facet_ s t) where
    type SpaceOf (Facet_ s t) = s

instance (Space s) => HasSpace (HardFacet_ s t) where
    type SpaceOf (HardFacet_ s t) = s

instance (Space s) => SimpleTransformable (FacetSide s t) where
    translateBy p = over sceneSide (translateBy p)
    scaleBy s     = over sceneSide (scaleBy s)
    stretchBy p   = over sceneSide (stretchBy p)

instance (Space s) => Transformable (FacetSide s t) where
    rotateBy a = over sceneSide (rotateBy a)

instance (Space s) => SimpleTransformable (Facet_ s t) where
    translateBy p = over facetSides (fmap (translateBy p))
    scaleBy s     = over facetSides (fmap (scaleBy s    ))
    stretchBy p   = over facetSides (fmap (stretchBy p  ))

instance (Space s) => Transformable (Facet_ s t) where
    rotateBy a    = over facetSides (fmap (rotateBy a   ))

instance (Space s, Space t) => CanProject (BezierSpace s) (Facet_ s t) where
    projectionWithStepsAccuracy = undefined

instance (Space s) => HasBox (Facet_ s t) where
    boxOf (Facet v3) = foldl1 minMaxBox (fmap (boxOf . view sceneSide) v3)

instance (Space s) => HasBox (HardFacet_ s t) where
    boxOf (HardFacet face _) = foldl1 minMaxBox (fmap boxOf face)


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

instance (NFData s, NFData t) => NFData (FacetSide s t) where
  rnf (FacetSide scene tex) = scene `deepseq` tex `deepseq` ()

instance (NFData s, NFData t) => NFData (Facet_ s t) where
  rnf (Facet thresholds) = thresholds `deepseq` ()

instance (NFData s, NFData t) => NFData (HardFacet_ s t) where
  rnf (HardFacet face tex) = face `deepseq` tex `deepseq` ()
