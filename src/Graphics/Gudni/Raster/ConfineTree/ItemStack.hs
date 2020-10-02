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

module Graphics.Gudni.Raster.ConfineTree.ItemStack
  ( toggleItem
  , combineItemStacks
  , crossCurveAlong
  , crossCurveBetweenPoints
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier

import Control.Lens

toggleItem :: ItemTagId -> ItemStack -> ItemStack
toggleItem itemTagId stack =
    case stack of
        (x:xs) | itemTagId <  x -> itemTagId:x:xs
               | itemTagId == x -> xs
               | itemTagId >  x -> x:toggleItem itemTagId xs
        [] -> [itemTagId]

combineItemStacks :: ItemStack -> ItemStack -> ItemStack
combineItemStacks = flip (foldl (flip toggleItem))

crossCurveAlong :: ( Axis axis
                   , Space s)
                => axis
                -> Along axis s
                -> Athwart axis s
                -> Along axis s
                -> TaggedBezier s
                -> ItemStack
                -> ItemStack
crossCurveAlong axis start baseline end (TaggedBezier bez _ itemTagId) =
    if crossesAlong axis start baseline end bez
    then toggleItem itemTagId
    else id

crossCurveBetweenPoints :: (Space s)
                        => Point2 s
                        -> Point2 s
                        -> ItemStack
                        -> TaggedBezier s
                        -> ItemStack
crossCurveBetweenPoints anchor point stack (TaggedBezier bez tag itemTagId) =
    if crosses anchor point bez
    then toggleItem itemTagId stack
    else stack

traverseCurve :: forall s m
              .  ( Space s
                 , Monad m
                 )
              => Point2 s
              -> Point2 s
              -> Box s
              -> TaggedBezier s
              -> ItemStack
              -> m ItemStack
traverseCurve anchor point box curve stack =
    let bez = curve ^. tBez
        bezBox = boxOf bez
    in
    if bezBox ^. maxBox . pX >= box ^. minBox . pX &&
       bezBox ^. maxBox . pY >= box ^. minBox . pY
    then return $ crossCurveBetweenPoints anchor point stack curve
    else return stack
