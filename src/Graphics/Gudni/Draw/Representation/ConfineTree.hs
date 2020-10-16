{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}


module Graphics.Gudni.Draw.Representation.ConfineTree
  ( constructConfineTree
  , constructConfineTreeBound
  , constructConfineTreeBoxed
  , confineTreeBox
  , constructConfine
  , reasonableBoundaries
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.Traverse
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Query
import Graphics.Gudni.Raster.Dag.State

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.ConfineQuery
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Foreign.Storable
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M

class (Axis axis) => AxisColor axis where
  axisColor :: axis -> Color

instance AxisColor Vertical where
  axisColor Vertical   = red

instance AxisColor Horizontal where
  axisColor Horizontal = blue

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

overlapBlock :: (Axis axis, Space s) => axis -> Athwart axis s -> Athwart axis s -> Box s -> Box s
overlapBlock axis cut overlap boundary =
    let minPoint = pointAlongAxis axis (boundary ^. minBox . along axis) cut
        maxPoint = pointAlongAxis axis (boundary ^. maxBox . along axis) overlap
    in Box minPoint maxPoint

axisLine :: (Axis axis, Space s) => axis -> Athwart axis s -> Box s -> Bezier s
axisLine axis cut boundary =
    let minPoint = pointAlongAxis axis (boundary ^. minBox . along axis) cut
        maxPoint = pointAlongAxis axis (boundary ^. maxBox . along axis) cut
    in  line (constrainPoint minPoint) (constrainPoint maxPoint)

bezierArrow :: (IsStyle style) => Bezier (SpaceOf style) -> Layout style
bezierArrow bz@(Bez v0 c v1) =
     let r = 5
         w = 10
         h = 7.5
     in
     overlap [ withColor black . place . withArrowHead (Point2 w h) PointingForward $ bz
             , withColor black . mask . stroke 1 $ makeOpenCurve [bz]
             ]

constructConfine :: forall axis style m
                 .  ( IsStyle style
                    , AxisColor axis
                    , Storable (SpaceOf style)
                    , MonadIO m
                    )
                 => axis
                 -> Confine axis (SpaceOf style)
                 -> Box (SpaceOf style)
                 -> FabricMonad (SpaceOf style) m (Layout style)
constructConfine axis tree boundary =
    let thickness :: SpaceOf style
        thickness = 1
        cut       = tree ^. confineCut
        overhang  = tree ^. confineOverhang
        aColor    = axisColor axis
        aLine :: Bezier (SpaceOf style)
        aLine = axisLine axis cut boundary
        axisLayout :: Layout style
        axisLayout = withColor (transparent 0.2 aColor) . mask . stroke thickness . makeOpenCurve $ [aLine]
        overhangBox :: Box (SpaceOf style)
        overhangBox = overlapBlock axis cut overhang boundary
        overhangLayout :: Layout style
        overhangLayout = withColor (transparent 0.01 aColor) . mask . boxToRectangle $ overhangBox
    in
    do  bez <- loadCurveS (tree ^. confinePrimTagId)
        let curve :: Layout style
            curve = bezierArrow bez
            text = blurb (show $ tree ^. confinePrimTagId)
            label :: Layout style
            label = translateBy (eval 0.5 bez) .
                    scaleBy 40 .
                    withColor (transparent 0.5 purple) $
                    text
        return $
            overlap $ [
                      -- label
                      -- ,
                      -- curve
                      -- ,
                      axisLayout
                      -- ,
                      -- overhangLayout
                      ]

constructConfineTreeBound :: forall style m
                          .  ( IsStyle style
                             , Storable (SpaceOf style)
                             , MonadIO m )
                          => Box (SpaceOf style)
                          -> ConfineTree (SpaceOf style)
                          -> FabricMonad (SpaceOf style) m (Layout style)
constructConfineTreeBound startBoundary =
  go Vertical 0 startBoundary
  where
  go :: ( Axis axis
        , AxisColor axis
        , AxisColor (PerpendicularTo axis)
        , axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Int
     -> Box (SpaceOf style)
     -> Branch axis (SpaceOf style)
     -> FabricMonad (SpaceOf style) m (Layout style)
  go axis depth boundary mTree =
        if widthOf boundary > 0 && heightOf boundary > 0
        then case mTree of
               Nothing   -> return emptyItem
               Just tree ->
                   do confine <- constructConfine axis tree boundary
                      let cut     = tree ^. confineCut
                          (lessBound, moreBound) = splitBox axis cut boundary
                      lessBranch <- go (perpendicularTo axis) (depth + 1) lessBound (tree ^. confineLessCut)
                      moreBranch <- go (perpendicularTo axis) (depth + 1) moreBound (tree ^. confineMoreCut)
                      return $ overlap [confine, lessBranch, moreBranch]
        else return emptyItem

constructConfineTree :: forall style m
                     .  ( IsStyle style
                        , Storable (SpaceOf style)
                        , MonadIO m )
                     => ConfineTree (SpaceOf style)
                     -> FabricMonad (SpaceOf style) m (Layout style)
constructConfineTree =
     constructConfineTreeBound reasonableBoundaries

constructConfineTreeBoxed :: forall style m
                          .  ( IsStyle style
                             , Storable (SpaceOf style)
                             , MonadIO m )
                          => ConfineTree (SpaceOf style)
                          -> FabricMonad (SpaceOf style) m (Layout style)
constructConfineTreeBoxed confineTree =
    do mBox <- confineTreeBox confineTree
       case mBox of
           Just box -> constructConfineTreeBound box confineTree
           Nothing  -> return emptyItem

confineTreeBox :: forall m s
               .  ( MonadIO m
                  , Space s
                  , Storable s
                  )
               => ConfineTree s
               -> FabricMonad s m (Maybe (Box s))
confineTreeBox =
  go Vertical
  where
  go :: ( Axis axis )
     => axis
     -> Branch axis s
     -> FabricMonad s m (Maybe (Box s))
  go axis mTree =
      case mTree of
          Nothing -> return Nothing
          Just tree ->
              do box <- boxOf <$> loadCurveS (tree ^. confinePrimTagId)
                 mLBox <- go (perpendicularTo axis) (tree ^. confineLessCut)
                 mGBox <- go (perpendicularTo axis) (tree ^. confineMoreCut)
                 return $ eitherMaybe minMaxBox (Just box) $ eitherMaybe minMaxBox mLBox mGBox
