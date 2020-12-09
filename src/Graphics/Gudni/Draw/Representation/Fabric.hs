{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Draw.Representation.Fabric
  ( constructDag
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Traverse
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Storage

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Symbols
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.ConfineTree

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Subdividable

import Foreign.Storable
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.PrettyPrint.GenericPretty

scaler :: IsStyle style => Layout style -> Layout style
scaler = scaleBy 30

constructDag :: ( MonadIO m
                , IsStyle style
                )
             => FabricTagId
             -> DagMonad (SpaceOf style) m (Layout style)
constructDag fabricTagId =
    error "not implemented for instruction version"
    {-
    do if fabricTagId == nullFabricTagId
       then return . withColor black $ hatch 1 30
       else do fabric <- loadFabricS fabricTagId
               case fabric of
                   FBinary ty a b ->
                       do aLayout <- constructDag a
                          bLayout <- constructDag b
                          return $ rack [ scaler . withColor blue . blurb $ show fabricTagId ++ " Comb " ++ show ty
                                        , stack [ aLayout
                                                , bLayout
                                                ]
                                        ]
                   FUnaryPre trans child ->
                       do childLayout <- constructDag child
                          return $ rack [ scaler . withColor blue . blurb $ show fabricTagId ++ " Trans " ++ show trans
                                        , childLayout
                                        ]
                   FUnaryPost trans child ->
                       do childLayout <- constructDag child
                          return $ rack [ scaler . withColor blue . blurb $ show fabricTagId ++ " Trans " ++ show trans
                                        , childLayout
                                        ]
                   FLeaf leaf ->
                       case leaf of
                           FTree rootId childId ->
                               do childLayout <- constructDag childId
                                  (confineTreeId, decorateTreeId) <- loadTreeRootS rootId
                                  mBox <- confineTreeBox confineTreeId
                                  constructedTree <-
                                      case mBox of
                                          Nothing  -> return emptyItem
                                          Just box -> constructConfineTreeBound box confineTreeId
                                  return $
                                      stack [ scaler . withColor (dark green) . blurb $ "Tree " ++ show confineTreeId ++ " mBox" ++ show mBox
                                            , scaler constructedTree
                                            , childLayout
                                            ]
                           FTreeSubstance substance ->
                               do subst <- constructSubstance substance
                                  return $ stack [ --scaler . withColor red . blurb $ show fabricTagId ++ " Subst"
                                                   -- ,
                                                   subst
                                                 ]
     -}
{-
constructSubstance :: ( MonadIO m
                      , IsStyle style
                      )
                   => FSubstance (ForStorage (SpaceOf style))
                   -> DagMonad (SpaceOf style) m (Layout style)
constructSubstance substance =
  case substance of
      FConst color -> return . scaleBy 30 . withColor color . mask $ circle
      FTexture   t -> return . withColor (blueish   gray) $ hatch 1 30
      FLinear      -> return . withColor (yellowish gray) $ hatch 1 30
      FQuadrance   -> return . withColor (yellowish gray) $ hatch 1 30
-}
