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
  , crossPrimAlong
  , crossPrimBetweenPoints
  , crossesPrimAlong
  , traversePrim
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

import Linear.Metric
import Control.Applicative
import Control.Lens

toggleItem :: PrimTagId -> PrimStack -> PrimStack
toggleItem primTagId stack =
    case stack of
        (x:xs) | primTagId <  x -> primTagId:x:xs
               | primTagId == x -> xs
               | primTagId >  x -> x:toggleItem primTagId xs
        [] -> [primTagId]

combineItemStacks :: PrimStack -> PrimStack -> PrimStack
combineItemStacks = flip (foldl (flip toggleItem))

crossPrimAlong :: ( Axis axis
                  , Space s)
               => axis
               -> Along axis s
               -> Athwart axis s
               -> Along axis s
               -> PrimTagId
               -> Primitive s
               -> PrimStack
               -> PrimStack
crossPrimAlong axis start baseline end primTagId prim =
    if crossesPrimAlong axis start baseline end prim
    then toggleItem primTagId
    else id

crossPrimBetweenPoints :: (Space s)
                       => Point2 s
                       -> Point2 s
                       -> PrimTagId
                       -> Primitive s
                       -> PrimStack
                       -> PrimStack
crossPrimBetweenPoints anchor point primTagId prim =
    if crossesPrim anchor point prim
    then toggleItem primTagId
    else id

traversePrim :: forall s m
             .  ( Space s
                , Monad m
                )
             => Point2 s
             -> Point2 s
             -> Box s
             -> PrimTagId
             -> Primitive s
             -> PrimStack
             -> m PrimStack
traversePrim anchor point box primTagId prim stack =
    let primBox = boxOf prim
    in
    if primBox ^. maxBox . pX >= box ^. minBox . pX &&
       primBox ^. maxBox . pY >= box ^. minBox . pY
    then return $ crossPrimBetweenPoints anchor point primTagId prim stack
    else return stack
