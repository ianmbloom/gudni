{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  , Facet(..)
  , facetSides
  , HardFacet_(..)
  , HardFacet(..)
  , hardFace
  , FacetSide(..)
  , hardTexture
  , sceneSide
  , textureSide
  , hardenFacet
  , tesselateFacet
  , tesselateFacetSteps
  , triangleToFacet
  , rectangleToFacets
  , FacetGroup_(..)
  , FacetGroup(..)
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Shape
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Util.Chain

import Linear.V2
import Linear.V3
import Linear.V4
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe


import Foreign.C.Types
import Foreign.Storable
import Graphics.Gudni.Util.StorableM

data FacetSide s = FacetSide
  { _sceneSide   :: Bezier s
  , _textureSide :: V2 (Point2 s)
  } deriving (Show)
makeLenses ''FacetSide

data Facet_ s = Facet
  { _facetSides :: V3 (FacetSide s)
  } deriving (Show)
makeLenses ''Facet_

type Facet = Facet_ SubSpace

newtype FacetGroup_ f s = FacetGroup {unFacetGroup :: f (Facet_ s)}
type FacetGroup s = FacetGroup_ ShapeFunctor s

instance Space s => HasSpace (FacetGroup_ f s) where
  type SpaceOf (FacetGroup_ f s) = s

data HardFacet_ s = HardFacet
  { _hardFace    :: V3 (Point2 s)
  , _hardTexture :: V3 (Point2 s)
  } deriving (Show)
makeLenses ''HardFacet_

type HardFacet = HardFacet_ SubSpace

shouldSubdivideBezier :: (Space s) => s -> Bezier s -> Bool
shouldSubdivideBezier tolerance bez =
  let midPoint = mid (bez ^. bzStart) (bez ^. bzEnd)
      tDistance = taxiDistance midPoint (bez ^. bzControl)
  in  tDistance > tolerance

shouldSubdivideFacet :: (Space s) => s -> Facet_ s -> Bool
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

splitLineSegment :: (Space s) => s -> V2 (Point2 s) -> (V2 (Point2 s), V2 (Point2 s))
splitLineSegment t (V2 v0 v1) = let midPoint = lerp t v0 v1 in (V2 v0 midPoint, V2 midPoint v1)

splitBezierSide :: (Space s) => s -> FacetSide s -> (FacetSide s, FacetSide s)
splitBezierSide t (FacetSide bez tex) =
  let (leftBez, rightBez) = splitBezier t bez
      (leftTex, rightTex) = splitLineSegment t tex
  in  (FacetSide leftBez leftTex, FacetSide rightBez rightTex)

sideTriangle :: (Space s)
             => Lens' (V3 s) s
             -> Lens' (V3 s) s
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> V3 s
             -> Facet_ s
             -> Facet_ s
sideTriangle leftSplit rightSplit left right bottom splitPoints facet =
  let (_, left')  = splitBezierSide (splitPoints ^. leftSplit)  (facet ^. facetSides . left)
      (right', _) = splitBezierSide (splitPoints ^. rightSplit) (facet ^. facetSides . right)
      midControl = mid (facet ^. facetSides . left . sceneSide . bzEnd) (facet ^. facetSides . bottom . sceneSide . bzControl)
      bottomBez = Bez (left' ^. sceneSide . bzStart) midControl (right' ^. sceneSide . bzEnd)
      bottomTex = V2 (left' ^. textureSide . _x) (right' ^. textureSide . _y)
      bottom' = FacetSide bottomBez bottomTex
  in  set (facetSides . left) left' . set (facetSides . right) right' . set (facetSides . bottom) bottom' $ facet


subdivideFacetT :: (Space s) => V3 s -> Facet_ s -> V4 (Facet_ s)
subdivideFacetT splitPoints facet =
  let triZ  = sideTriangle _x _y   _x _y _z splitPoints facet
      triX  = sideTriangle _y _z   _y _z _x splitPoints facet
      triY  = sideTriangle _z _x   _z _x _y splitPoints facet
      triIn = Facet {_facetSides = V3 (triZ ^. facetSides . _z)
                                      (triX ^. facetSides . _x)
                                      (triY ^. facetSides . _y)
                    }
  in  V4 triX triY triZ triIn

subdivideFacet :: (Space s, Alternative f) => Facet_ s -> f (Facet_ s)
subdivideFacet = foldl1 (<|>) . fmap pure . subdivideFacetT (pure hALF)

maybeSubdivideFacet :: (Space s, Alternative f) => (Bezier s -> Maybe s) -> Facet_ s -> Maybe (f (Facet_ s))
maybeSubdivideFacet f facet =
    let mSplitPoints = fmap (f . view sceneSide) $ facet ^. facetSides
    in  if or (fmap isJust mSplitPoints)
        then let splitPoints = fmap (fromMaybe hALF) mSplitPoints
                 (V4 triZ triX triY triIn) = subdivideFacetT splitPoints facet
             in  Just $ pure triX <|> pure triY <|> pure triZ <|> pure triIn
        else Nothing

maybeCutFacet :: (Space s) => (Bezier s -> Maybe s) -> Facet_ s -> Maybe (FacetGroup s, FacetGroup s)
maybeCutFacet f facet =
    let mSplitPoints = fmap (f . view sceneSide) $ facet ^. facetSides
    in  if or (fmap isJust mSplitPoints)
        then let splitPoints = fmap (fromMaybe hALF) mSplitPoints
                 (V4 triX triY triZ triIn) = subdivideFacetT splitPoints facet
             in  case mSplitPoints of
                    V3 Nothing  (Just _) (Just _) -> Just (FacetGroup $ pure triX, FacetGroup $ pure triY <|> pure triZ <|> pure triIn)
                    V3 (Just _) Nothing  (Just _) -> Just (FacetGroup $ pure triY, FacetGroup $ pure triX <|> pure triZ <|> pure triIn)
                    V3 (Just _) (Just _) Nothing  -> Just (FacetGroup $ pure triZ, FacetGroup $ pure triX <|> pure triY <|> pure triIn)
                    _ -> error "split not quite working"
        else Nothing

instance (Space s) => CanDeknob (Facet_ s) where
    deKnob axis = maybeSubdivideFacet (maybeKnobSplitPoint axis)

instance Space s => CanCut (FacetGroup s) where
    -- | Split item across horizontal or vertical line
    splitAtCut axis splitPoint = undefined . fmap (maybeCutFacet (maybeCutPointBezier axis splitPoint)) . unFacetGroup
    -- | Determine if horizontal or vertical line cuts item
    canCut axis splitPoint = undefined -- or . join . fmap (fmap (canCut axis splitPoint . view sceneSide) . view facetSides) . unFacetGroup

tesselateFacet :: (Space s)
               => s
               -> FacetGroup s
               -> FacetGroup s
tesselateFacet tolerance (FacetGroup facets) =
  FacetGroup $ go facets
  where
  go facets =
     let subFacets = join . fmap subdivideFacet $ facets
         (continued, done) = segregate (shouldSubdivideFacet tolerance) subFacets
         rest = go continued
     in  (rest <|> done)

tesselateFacetSteps :: (Space s)
                    => Int
                    -> FacetGroup s
                    -> FacetGroup s
tesselateFacetSteps steps (FacetGroup facets) = FacetGroup $
  go steps facets
  where
  go steps facets =
     if steps > 0
     then go (steps - 1) $ join . fmap subdivideFacet $ facets
     else facets

stripBezier :: Bezier s -> V2 (Point2 s)
stripBezier (Bez v0 c v1) = V2 v0 v1

hardenFacet :: Facet_ s -> HardFacet_ s
hardenFacet facet =
    let sceneFacet = fmap (view _x . stripBezier . view sceneSide) . view facetSides $ facet
        textureFacet = fmap (view (textureSide . _x)) . view facetSides $ facet
    in  HardFacet { _hardFace = sceneFacet, _hardTexture = textureFacet}

linesToFacetSide :: Space s => Point2 s -> Point2 s -> Point2 s -> Point2 s -> FacetSide s
linesToFacetSide p0 p1 t0 t1 = FacetSide (line p0 p1) (V2 t0 t1)

triangleToFacet :: Space s => V3 (Point2 s) -> V3 (Point2 s) -> Facet_ s
triangleToFacet (V3 p0 p1 p2) (V3 t0 t1 t2) =
  Facet $
  V3 (linesToFacetSide p0 p1 t0 t1)
     (linesToFacetSide p1 p2 t1 t2)
     (linesToFacetSide p2 p0 t2 t0)

rectangleToFacets :: (Space s, Alternative f) => Box s -> f (Facet_ s)
rectangleToFacets box =
      pure (triangleToFacet (V3 (box ^. topLeftBox) (box ^. topRightBox) (box ^. bottomLeftBox))
                            (V3 (box ^. topLeftBox) (box ^. topRightBox) (box ^. bottomLeftBox))
           )
      <|>
      pure (triangleToFacet (V3 (box ^. bottomRightBox) (box ^. bottomLeftBox) (box ^. topRightBox))
                            (V3 (box ^. bottomRightBox) (box ^. bottomLeftBox) (box ^. topRightBox))
           )

instance (Space s) => HasSpace (FacetSide s) where
    type SpaceOf (FacetSide s) = s

instance (Space s) => HasSpace (Facet_ s) where
    type SpaceOf (Facet_ s) = s

instance (Space s) => HasSpace (HardFacet_ s) where
    type SpaceOf (HardFacet_ s) = s

instance (Space s) => CanBox (Facet_ s) where
    boxOf (Facet v3) = foldl1 minMaxBox (fmap (boxOf . view sceneSide) v3)

instance (Space s) => CanBox (HardFacet_ s) where
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

instance (NFData s) => NFData (FacetSide s) where
  rnf (FacetSide scene tex) = scene `deepseq` tex `deepseq` ()

instance (NFData s) => NFData (Facet_ s) where
  rnf (Facet thresholds) = thresholds `deepseq` ()

instance (NFData s) => NFData (HardFacet_ s) where
  rnf (HardFacet face tex) = face `deepseq` tex `deepseq` ()
