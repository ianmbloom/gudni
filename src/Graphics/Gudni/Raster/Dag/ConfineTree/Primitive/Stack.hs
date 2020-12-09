{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Stack
  ( ShapeStack(..)
  , toggleShapeActive
  , combineShapeStacks
  , passPrimAlong
  , passPrimBetweenPoints
  , passPrim
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Cross
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Util.Debug

import Linear.Metric
import Control.Applicative
import Control.Lens

type ShapeStack = [FabricTagId]

toggleShapeActive :: FabricTagId -> ShapeStack -> ShapeStack
toggleShapeActive tagId stack =
    case stack of
        (x:xs) | tagId >  x -> tagId:x:xs
               | tagId == x -> xs
               | tagId <  x -> x:toggleShapeActive tagId xs
        [] -> [tagId]

combineShapeStacks :: ShapeStack -> ShapeStack -> ShapeStack
combineShapeStacks = flip (foldl (flip toggleShapeActive))

passPrimAlong :: ( Axis axis
                 , Space s)
              => s
              -> axis
              -> Along axis s
              -> Athwart axis s
              -> Along axis s
              -> PrimTagId
              -> Primitive s
              -> ShapeStack
              -> ShapeStack
passPrimAlong limit axis start baseline end primTagId prim =
    if crossesPrimAlong limit axis start baseline end prim
    then toggleShapeActive (prim ^. primFabricTagId)
    else id

passPrimBetweenPoints :: (Space s)
                      => s
                      -> Point2 s
                      -> Point2 s
                      -> PrimTagId
                      -> Primitive s
                      -> ShapeStack
                      -> ShapeStack
passPrimBetweenPoints limit anchor point primTagId prim =
    if crossesPrim limit anchor point prim
    then toggleShapeActive (prim ^. primFabricTagId)
    else id

passPrim :: forall s m
         .  ( Space s
            , Monad m
            )
         => s
         -> Point2 s
         -> Point2 s
         -> Box s
         -> PrimTagId
         -> Primitive s
         -> ShapeStack
         -> m ShapeStack
passPrim limit anchor point box primTagId prim stack =
    let primBox = boxOf prim
    in
    if primBox ^. maxBox . pX >= box ^. minBox . pX &&
       primBox ^. maxBox . pY >= box ^. minBox . pY
    then return $ passPrimBetweenPoints limit anchor point primTagId prim stack
    else return stack
