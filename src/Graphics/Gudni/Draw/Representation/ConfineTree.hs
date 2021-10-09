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
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Traverse
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.TagTypes

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.Primitive
import Graphics.Gudni.Draw.Representation.RayQuery
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
  axisColor :: Space s => axis -> Color s

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

bezierArrow :: (IsStyle style) => Bezier (SpaceOf style) -> Layout Rgba style
bezierArrow bz@(Bez v0 c v1) =
     let r = 5
         w = 10
         h = 7.5
     in
     overlap [ withColor black . withArrowHead (Point2 w h) PointingForward $ bz
             , withColor black . place . stroke 1 $ makeOpenCurve [bz]
             ]

constructConfine :: forall axis style m
                 .  ( IsStyle style
                    , AxisColor axis
                    , Storable (SpaceOf style)
                    , MonadIO m
                    )
                 => axis
                 -> ConfineTag (SpaceOf style)
                 -> Box (SpaceOf style)
                 -> TreeMonad (SpaceOf style) m (Layout Rgba style)
constructConfine axis tree boundary =
    let thickness :: SpaceOf style
        thickness = 1
        cut       = toAthwart axis (tree ^. confineTagCut     )
        overhang  = toAthwart axis (tree ^. confineTagOverhang)
        aColor    = axisColor axis
        aLine :: Bezier (SpaceOf style)
        aLine = axisLine axis cut boundary
        axisLayout :: Layout Rgba style
        axisLayout = withColor (transparent 0.2 aColor) . place . stroke thickness . makeOpenCurve $ [aLine]
        overhangBox :: Box (SpaceOf style)
        overhangBox = overlapBlock axis cut overhang boundary
        overhangLayout :: Layout Rgba style
        overhangLayout = withColor (transparent 0.01 aColor) . place . boxToRectangle $ overhangBox
    in
    do  prim <- loadTreePrim (tree ^. confineTagPrimTagId)
        let primLayout :: Layout Rgba style
            primLayout = withColor blue . drawPrim $ prim
            label :: Layout Rgba style
            label = withColor (transparent 0.5 purple) . labelPrim (tree ^. confineTagPrimTagId) $ prim
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
                          -> ConfineTagId (SpaceOf style)
                          -> TreeMonad (SpaceOf style) m (Layout Rgba style)
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
     -> ConfineTagId (SpaceOf style)
     -> TreeMonad (SpaceOf style) m (Layout Rgba style)
  go axis depth boundary treeId =
        if widthOf boundary > 0 && heightOf boundary > 0
        then if treeId == nullConfineTagId
             then return emptyItem
             else do  tree <- loadConfineTag treeId
                      confine <- constructConfine axis tree boundary
                      let cut     = toAthwart axis (tree ^. confineTagCut)
                          (lessBound, moreBound) = splitBox axis cut boundary
                      lessBranch <- go (perpendicularTo axis) (depth + 1) lessBound (tree ^. confineTagLessCut)
                      moreBranch <- go (perpendicularTo axis) (depth + 1) moreBound (tree ^. confineTagMoreCut)
                      return $ overlap [confine, lessBranch, moreBranch]
        else return emptyItem

constructConfineTree :: forall style m
                     .  ( IsStyle style
                        , Storable (SpaceOf style)
                        , MonadIO m )
                     => ConfineTagId (SpaceOf style)
                     -> TreeMonad (SpaceOf style) m (Layout Rgba style)
constructConfineTree =
     constructConfineTreeBound reasonableBoundaries

constructConfineTreeBoxed :: forall style m
                          .  ( IsStyle style
                             , Storable (SpaceOf style)
                             , MonadIO m )
                          => ConfineTagId (SpaceOf style)
                          -> TreeMonad (SpaceOf style) m (Layout Rgba style)
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
               => ConfineTagId s
               -> TreeMonad s m (Maybe (Box s))
confineTreeBox =
  go Vertical
  where
  go :: ( Axis axis )
     => axis
     -> ConfineTagId s
     -> TreeMonad s m (Maybe (Box s))
  go axis treeId =
      if treeId == nullConfineTagId
      then return Nothing
      else  do  tree <- loadConfineTag treeId
                box <- boxOf <$> loadTreePrim (tree ^. confineTagPrimTagId)
                mLBox <- go (perpendicularTo axis) (tree ^. confineTagLessCut)
                mGBox <- go (perpendicularTo axis) (tree ^. confineTagMoreCut)
                return $ eitherMaybe minMaxBox (Just box) $ eitherMaybe minMaxBox mLBox mGBox
