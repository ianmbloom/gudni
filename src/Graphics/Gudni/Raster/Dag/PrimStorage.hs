{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Gudni.Raster.Dag.PrimStorage
  ( PrimStorage(..)
  , primBezierPile
  , primFacetPile
  , primTagPile
  , initPrimStorage
  , freePrimStorage
  , storePrim
  , loadPrim
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Thresholds.SubstanceInfo
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive
import Graphics.Gudni.Raster.Dag.PrimTag

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

data PrimStorage s
    = PrimStorage
      -- | A list of every bezier that defines the scene.
    { _primBezierPile :: Pile (Bezier s)
      -- | A list of texture facets collected from the scene.
    , _primFacetPile  :: Pile (Facet s)
      -- | A pile of every prim tag collected from the scene.
    , _primTagPile    :: Pile PrimTag
    }
makeLenses ''PrimStorage

initPrimStorage :: ( Storable s
                   , MonadIO m)
                   => m (PrimStorage s)
initPrimStorage =
   do bezierPile  <- newPile
      facetPile   <- newPile
      primTagPile <- newPile
      return $ PrimStorage
               { _primBezierPile  = bezierPile
               , _primFacetPile   = facetPile
               , _primTagPile = primTagPile
               }

freePrimStorage :: ( Storable s
                   , MonadIO m)
                => PrimStorage s -> m ()
freePrimStorage storage =
  do freePile $ storage ^. primBezierPile
     freePile $ storage ^. primFacetPile
     freePile $ storage ^. primTagPile

storePrim :: ( Storable s
             , MonadIO m)
          => Primitive s
          -> StateT (PrimStorage s) m (PrimTagId)
storePrim prim =
  case prim of
      PrimBezier fabricTagId bez ->
          do bezId <- addToPileS primBezierPile bez
             let tag = makeBezierPrimTag (BezierId bezId) fabricTagId
             primTagId <- addToPileS primTagPile tag
             return $ PrimTagId primTagId
      PrimFacet fabricTagId facet ->
          do facetId <- addToPileS primFacetPile facet
             let tag = makeFacetPrimTag (FacetId facetId) fabricTagId
             primTagId <- addToPileS primTagPile tag
             return $ PrimTagId primTagId

loadPrim :: ( Storable s
            , MonadIO m
            )
         => PrimTagId
         -> StateT (PrimStorage s) m (Primitive s)
loadPrim primTagId =
  do tag <- fromPileS primTagPile (unPrimTagId primTagId)
     let buildPrim
             | primTagIsBezier tag = do let fabricTagId = primTagFabricTagId tag
                                        bez <- fromPileS primBezierPile (unBezierId . primTagBezierId $ tag)
                                        return $ PrimBezier fabricTagId bez
             | primTagIsFacet  tag = do let fabricTagId = primTagFabricTagId tag
                                        facet <- fromPileS primFacetPile (unFacetId . primTagFacetId $ tag)
                                        return $ PrimFacet fabricTagId facet
             | otherwise = error "unsupported primType"
     buildPrim
