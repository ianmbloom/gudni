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

module Graphics.Gudni.Raster.Dag.Primitive.Stack
  ( toggleItem
  , combineItemStacks
  , passPrimAlong
  , passPrimBetweenPoints
  , passPrim
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Cross
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Util.Debug

import Linear.Metric
import Control.Applicative
import Control.Lens

toggleItem :: ShapeId -> ShapeStack -> ShapeStack
toggleItem shapeId stack =
    case stack of
        (x:xs) | shapeId <  x -> shapeId:x:xs
               | shapeId == x -> xs
               | shapeId >  x -> x:toggleItem shapeId xs
        [] -> [shapeId]

combineItemStacks :: ShapeStack -> ShapeStack -> ShapeStack
combineItemStacks = flip (foldl (flip toggleItem))

passPrimAlong :: ( Axis axis
                 , Space s)
              => axis
              -> Along axis s
              -> Athwart axis s
              -> Along axis s
              -> PrimTagId
              -> Primitive s
              -> ShapeStack
              -> ShapeStack
passPrimAlong axis start baseline end primTagId prim =
    if crossesPrimAlong axis start baseline end prim
    then toggleItem (prim ^. primShapeId)
    else id

passPrimBetweenPoints :: (Space s)
                      => Point2 s
                      -> Point2 s
                      -> PrimTagId
                      -> Primitive s
                      -> ShapeStack
                      -> ShapeStack
passPrimBetweenPoints anchor point primTagId prim =
    if crossesPrim anchor point prim
    then toggleItem (prim ^. primShapeId)
    else id

passPrim :: forall s m
         .  ( Space s
            , Monad m
            )
         => Point2 s
         -> Point2 s
         -> Box s
         -> PrimTagId
         -> Primitive s
         -> ShapeStack
         -> m ShapeStack
passPrim anchor point box primTagId prim stack =
    let primBox = boxOf prim
    in
    if primBox ^. maxBox . pX >= box ^. minBox . pX &&
       primBox ^. maxBox . pY >= box ^. minBox . pY
    then return $ passPrimBetweenPoints anchor point primTagId prim stack
    else return stack
