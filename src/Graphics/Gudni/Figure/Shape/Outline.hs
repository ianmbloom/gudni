{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Shape.Outline
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for defining Outlines which are closed bezier curves.

module Graphics.Gudni.Figure.Shape.Outline
  ( Outline(..)
  , Outline_(..)
  , outlineSegments
  , makeOutline
  , closeOpenCurve
  , windingIsClockwise
  , windClockwise
  )
where

import Graphics.Gudni.Base.Reversible
import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Base.Loop

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Point
import Graphics.Gudni.Figure.Primitive.Box
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Figure.Shape.OpenCurve

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Control.Lens
import Linear.V2
import Linear.V3
import qualified Data.Vector as V

import Control.Applicative
import Data.Hashable
import Control.DeepSeq
import Control.Monad

-- | An shape is just a wrapper for a list of beziers. It represents one curve loop∘
newtype Outline_ f s = Outline
  { _outlineSegments :: f (Bezier s)
  }
makeLenses ''Outline_

makeOutline :: [Bezier s] -> Outline s
makeOutline list = Outline . V.fromList $ list

deriving instance (Eq   (f (Bezier s))) => Eq (Outline_ f s)
deriving instance (Ord  (f (Bezier s))) => Ord (Outline_ f s)
instance (Show (f (Bezier s))) => Show (Outline_ f s) where
  show (Outline vs) = "Outline" ++ show vs

type Outline s = Outline_ V.Vector s

instance ( Chain f
         , Space s)
         => PointContainer (Outline_ f s)
   where
   mapOverPoints f = over outlineSegments (fmap (over bzPoints (fmap f)))

instance ( Chain f
         , Space s
         )
         => BezierContainer (Outline_ f s)
  where
  type BezFunctor (Outline_ f s) = f
  joinOverBeziers f = Outline .  join . fmap f . view outlineSegments

-- | Close an open curve and convert it to an shape. An additional line segment is added if the outset and the terminator of
-- the curve are not the same.
closeOpenCurve :: forall f s . (Chain f, Space s) => OpenCurve_ f s -> Outline_ f s
closeOpenCurve curve =
  let connect :: f (Bezier s) -> f (Bezier s)
      connect = if curve ^. terminator == curve ^. outset
                 then id  -- if the beggining of the curve is the same as the end, ignore the end
                 else (pure (line (curve ^. terminator) (curve ^. outset)) <|>)
                     -- else insert a line segment from the end to the beggining.
  in  Outline . connect . view curveSegments $ curve

windBezierComponent :: Num s => Bezier s -> s
windBezierComponent b = (b ^. bzEnd . pX - b ^. bzStart . pX) * ((b ^. bzEnd . pY) + (b ^. bzStart . pY))

windingIsClockwise :: (Loop f, Space s) => Outline_ f s -> Bool
windingIsClockwise = (<0) .
                     sum .
                     fmap windBezierComponent .
                     view outlineSegments

windClockwise :: (Loop f, Reversible (f (Bezier s)), Space s, Show (f (Bezier s))) => Outline_ f s -> Outline_ f s
windClockwise outline = if (tr "windingIsClockWise" $ windingIsClockwise $ tr "outline" outline) then outline else reverseItem outline

pointInsideOutline :: (Loop f) => Space s => Outline_ f s -> Point2 s -> Bool
pointInsideOutline poly point =
    foldl (/=) False .
    fmap (
        \ bez ->
            let i = bez ^. bzStart
                j = bez ^. bzEnd
            in  (
                   (
                      (
                        (i ^. pY <= point ^. pY) &&
                        (point ^. pY < j ^. pY )
                      ) ||
                      ( (j ^. pY <= point ^. pY) &&
                        (point ^. pY < i ^. pY)
                      )
                   ) &&
                   (point ^. pX < (j ^. pX - i ^. pX) * (point ^. pY - i ^. pY) / (j ^. pY - i ^. pY) + i ^. pX)
                )
        ) .
    view outlineSegments $
    poly

instance Chain f => Reversible (Outline_ f s) where
  reverseItem = over outlineSegments (fmap reverseItem . reverseChain)

-- * Instances
instance Space s => HasSpace (Outline_ f s) where
  type SpaceOf (Outline_ f s) = s

instance (Chain f, Space s) => CanBox (Outline_ f s) where
  boxOf = minMaxBoxes . fmap boxOf . view outlineSegments

instance (NFData s, NFData (t (Bezier s))) => NFData (Outline_ t s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s vector = V.foldl hashWithSalt s vector

instance (Hashable (f (Bezier s))) => Hashable (Outline_ f s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
