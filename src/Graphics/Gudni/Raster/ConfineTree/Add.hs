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

module Graphics.Gudni.Raster.ConfineTree.Add
  ( addBezierToConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.Debug
import Control.Lens

addBezierToConfineTree :: forall s
                       . (Space s)
                       => TaggedBezier s
                       -> ConfineTree s
                       -> ConfineTree s
addBezierToConfineTree taggedBezier =
    go Vertical
    where
    box :: Box s
    box = boxOf (taggedBezier ^. tBez)

    go :: (Axis axis)
       => axis
       -> Maybe (Confine axis s)
       -> Maybe (Confine axis s)
    go axis mTree =
        --trP "goInsert" $
        let minCut = box ^. minBox . athwart axis
            maxCut = box ^. maxBox . athwart axis
        in
        case mTree of
            Nothing ->
                Just $
                Confine
                    { _confineCurve      = taggedBezier
                    , _confineCut        = minCut
                    , _confineOverhang   = minCut
                    , _confineLessCut    = Nothing
                    , _confineMoreCut    = Nothing
                    }
            Just tree ->
                if --tr ("minCut " ++ show minCut ++ " < tree ^. confineCut " ++ show (tree ^. confineCut)) $
                   minCut < tree ^. confineCut
                then let less = go (perpendicularTo axis) (tree ^. confineLessCut)
                     in  Just .
                         over confineOverhang (max maxCut) .
                         set confineLessCut less $
                         tree
                else let more = go (perpendicularTo axis) (tree ^. confineMoreCut)
                     in  Just .
                         set confineMoreCut more $
                         tree
