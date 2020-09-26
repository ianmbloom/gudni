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

module Graphics.Gudni.Raster.ConfineTree.Build
  ( addBezierToConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.Util(breakVector, clamp)
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Lens
import Data.Kind

import qualified Data.Vector as V
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

addBezierToConfineTree :: forall s . (Space s)
                       => ItemTagId
                       -> CurveTag
                       -> Bezier s
                       -> ConfineTree s
                       -> ConfineTree s
addBezierToConfineTree itemTagId tag bez =
    --tcP ("addBezierToConfineTree itemTagId " ++ show itemTagId ++ " tag " ++ show tag ++ " bez " ++ show bez) .
    goInsert Vertical
    where
    box :: Box s
    box = boxOf bez

    goInsert :: (Axis axis) => axis -> Maybe (Confine axis s) -> Maybe (Confine axis s)
    goInsert axis mTree =
        --trP "goInsert" $
        let minCut = onAxis axis (box ^. minBox . athwart axis)
            maxCut = onAxis axis (box ^. maxBox . athwart axis)
        in
        case mTree of
            Nothing ->
                Just $
                Confine
                    { _confineCurveTag   = tag
                    , _confineCurve      = bez
                    , _confineItemTagId  = itemTagId
                    , _confineCrossings  = []
                    , _confineCrossedCurves = []
                    , _confineCut        = minCut
                    , _confineOverhang   = minCut
                    , _confineLessCut    = Nothing
                    , _confineMoreCut    = Nothing
                    }
            Just tree ->
                Just $
                if tr ("minCut " ++ show minCut ++ " < tree ^. confineCut " ++ show (tree ^. confineCut)) $ minCut < tree ^. confineCut
                then over confineOverhang (max maxCut) .
                     set confineLessCut (goInsert (nextAxis axis) (tree ^. confineLessCut)) $
                     tree
                else set confineMoreCut (goInsert (nextAxis axis) (tree ^. confineMoreCut)) $
                     tree
