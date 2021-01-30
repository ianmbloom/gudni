{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.ConfineTree.Primitive.Storage
  ( PrimStorage(..)
  , primBezierPile
  , primFacetPile
  , primBoxPile
  , primTagPile
  , initPrimStorage
  , freePrimStorage
  , storePrim
  , loadPrim
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Figure.StorableInstances
import Graphics.Gudni.Raster.ConfineTree.Primitive.Constants
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Tag

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Util.Debug

import Foreign.Storable
import Foreign.Ptr
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

data PrimStorage s
    = PrimStorage
      -- | A list of every bezier that defines the scene.
    { _primBezierPile :: Pile (Bezier s)
      -- | A list of texture facets collected from the scene.
    , _primFacetPile  :: Pile (Facet s)
     -- | A list of boxes used by both rectangle and elipse prims collected from the scene.
    , _primBoxPile    :: Pile (Box s)
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
      boxPile     <- newPile
      primTagPile <- newPile
      return $ PrimStorage
               { _primBezierPile  = bezierPile
               , _primFacetPile   = facetPile
               , _primBoxPile     = boxPile
               , _primTagPile     = primTagPile
               }

freePrimStorage :: ( Storable s
                   , MonadIO m)
                => PrimStorage s -> m ()
freePrimStorage storage =
  do freePile $ storage ^. primBezierPile
     freePile $ storage ^. primFacetPile
     freePile $ storage ^. primBoxPile
     freePile $ storage ^. primTagPile

storePrim :: ( Storable s
             , MonadIO m)
          => Primitive s
          -> StateT (PrimStorage s) m (PrimTagId)
storePrim (Prim fabricTagId primType) =
  do tag <- case primType of
                PrimBezier bez ->
                    do bezId <- addToPileS primBezierPile bez
                       return $ makeBezierPrimTag (BezierId bezId) fabricTagId
                PrimFacet facet ->
                    do facetId <- addToPileS primFacetPile facet
                       return $ makeFacetPrimTag (FacetId facetId) fabricTagId
                PrimRect box ->
                    do boxId <- addToPileS primBoxPile box
                       return $ makeRectPrimTag (BoxId boxId) fabricTagId
                PrimEllipse box ->
                    do boxId <- addToPileS primBoxPile box
                       return $ makeElipsePrimTag (BoxId boxId) fabricTagId
     primTagId <- addToPileS primTagPile tag
     return $ PrimTagId primTagId

loadPrim :: ( Storable s
            , MonadIO m
            )
         => PrimTagId
         -> StateT (PrimStorage s) m (Primitive s)
loadPrim primTagId =
  do tag <- fromPileS primTagPile (unPrimTagId primTagId)
     let fabricTagId = primTagFabricTagId tag
         buildType
             | primTagIsBezier tag = do bez <- fromPileS primBezierPile (unBezierId . primTagBezierId $ tag)
                                        return $ PrimBezier  bez
             | primTagIsFacet  tag = do facet <- fromPileS primFacetPile (unFacetId . primTagFacetId $ tag)
                                        return $ PrimFacet   facet
             | primTagIsRect   tag = do box <- fromPileS primBoxPile (unBoxId . primTagBoxId $ tag)
                                        return $ PrimRect    box
             | primTagIsElipse tag = do box <- fromPileS primBoxPile (unBoxId . primTagBoxId $ tag)
                                        return $ PrimEllipse box
             | otherwise = error "unsupported primType"
     primType <- buildType
     return $ Prim fabricTagId primType
