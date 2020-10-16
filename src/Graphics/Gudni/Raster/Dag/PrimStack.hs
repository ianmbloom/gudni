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

module Graphics.Gudni.Raster.Dag.PrimStack
  ( toggleItem
  , combineItemStacks
  , crossCurveAlong
  , crossCurveBetweenPoints
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier

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

crossCurveAlong :: ( Axis axis
                   , Space s)
                => axis
                -> Along axis s
                -> Athwart axis s
                -> Along axis s
                -> PrimTagId
                -> Bezier s
                -> PrimStack
                -> PrimStack
crossCurveAlong axis start baseline end primTagId bez =
    if crossesAlong axis start baseline end bez
    then toggleItem primTagId
    else id

crossCurveBetweenPoints :: (Space s)
                        => Point2 s
                        -> Point2 s
                        -> PrimTagId
                        -> Bezier s
                        -> PrimStack
                        -> PrimStack
crossCurveBetweenPoints anchor point primTagId bez =
    if crosses anchor point bez
    then toggleItem primTagId
    else id

traverseCurve :: forall s m
              .  ( Space s
                 , Monad m
                 )
              => Point2 s
              -> Point2 s
              -> Box s
              -> PrimTagId
              -> Bezier s
              -> PrimStack
              -> m PrimStack
traverseCurve anchor point box primTagId bez stack =
    let bezBox = boxOf bez
    in
    if bezBox ^. maxBox . pX >= box ^. minBox . pX &&
       bezBox ^. maxBox . pY >= box ^. minBox . pY
    then return $ crossCurveBetweenPoints anchor point primTagId bez stack
    else return stack
