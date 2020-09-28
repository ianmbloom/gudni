{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}


module Graphics.Gudni.Draw.Representation.ConfineTree
  ( constructConfineTree
  , constructConfine
  , reasonableBoundaries
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Traverse

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.ConfineQuery
import Graphics.Gudni.Util.Debug

import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

squareAround :: forall s . Space s => s -> s -> CompoundTree s
squareAround thickness size = translateBy (pure ((-size)/2)) . mask . strokeOffset 0 thickness $ (rectangle (pure size) :: Shape s)

dec :: Maybe Int -> Maybe Int
dec x = subtract 1 <$> x

testDepth :: Maybe Int -> Bool
testDepth depth = isNothing depth || fromJust depth > 0

boxCenter box = ((box ^. topLeftBox) + (box ^. bottomRightBox)) / 2

class (Axis axis, AxisConstruction (NextAxis axis)) => AxisConstruction axis where
  axisColor    :: axis -> Color
  overlapBlock :: Space s => axis -> s -> s -> Box s -> Box s
  axisLine     :: Space s => axis -> s -> Box s -> Bezier s

instance AxisConstruction Vertical where
  axisColor    Vertical  = red
  axisLine     Vertical cut boundary = line (Point2 cut (boundary ^. topSide + 4)) (Point2 cut (boundary ^. bottomSide - 4))
  overlapBlock Vertical cut overlap boundary = makeBox cut (boundary ^. topSide) overlap (boundary ^. bottomSide)

instance AxisConstruction Horizontal where
  axisColor Horizontal   = blue
  axisLine Horizontal     cut boundary = line (Point2 (boundary ^. leftSide + 4) cut) (Point2 (boundary ^. rightSide - 4) cut)
  overlapBlock Horizontal cut overlap boundary = makeBox (boundary ^. leftSide) cut (boundary ^. rightSide) overlap

bezierArrow :: (IsStyle style) => Bezier (SpaceOf style) -> Layout style
bezierArrow bz@(Bez v0 c v1) =
     let r = 5
         w = 10
         h = 7.5
     in
     overlap [ withColor black . place . withArrowHead (Point2 w h) PointingForward $ bz
             --, withColor red   $ translateBy v0 $ closedCircle r
             --, withColor (light blue)  $ translateBy  c $ openCircle r
             --, withColor red   $ translateBy v1 $ closedCircle r
             , withColor black . mask . stroke 1 $ makeOpenCurve [bz]
             ]

rectangleAround :: IsStyle style => Point2 (SpaceOf style) -> Layout style
rectangleAround point =
  let size = 16
  in  withColor black . translateBy point . translateBy (pure (-size/2)) . mask $ openRectangle 2 (pointToBox (pure size))

constructConfine :: forall axis style
                 .  ( IsStyle style
                    , AxisConstruction axis
                    )
                 => axis
                 -> M.Map ItemTagId Color
                 -> [ItemTagId]
                 -> With axis            (SpaceOf style)
                 -> With (NextAxis axis) (SpaceOf style)
                 -> Confine axis (SpaceOf style)
                 -> Box (SpaceOf style)
                 -> Layout style
constructConfine axis colorMap layers parentCut parentLine tree boundary =
    let thickness :: SpaceOf style
        thickness = 1
        cut       = tree ^.  confineCut
        overhang  = tree ^.  confineOverhang
        aColor    = axisColor axis
        aLine :: Bezier (SpaceOf style)
        aLine = axisLine axis (fromAxis axis cut) boundary
        overhangBox :: Box (SpaceOf style)
        overhangBox = overlapBlock axis (fromAxis axis cut) (fromAxis axis overhang) boundary

        axisShape :: Layout style
        axisShape = withColor (transparent 0.2 aColor) . mask . stroke thickness . makeOpenCurve $ [aLine]
        overhangShape :: Layout style
        overhangShape = withColor (transparent 0.01 aColor) . mask . boxToRectangle $ overhangBox
        curve :: Layout style
        curve = bezierArrow $ (tree ^. confineCurve)
        text = blurb (show $ tree ^. confineCurveTag)
        label :: Layout style
        label = translateBy (eval 0.5 (tree ^. confineCurve)) .
                scaleBy 40 .
                withColor (transparent 0.5 purple) $
                text
        corner :: Layout style
        corner = constructCorner axis colorMap layers parentCut parentLine tree
    in  overlap $ [
                  --  label
                  --,
                  --curve
                  --,
                  -- axisShape
                  {-,overhangShape-}
                  --,
                  corner
                  ]

constructCorner  :: forall axis style
                 .  ( IsStyle style
                    , AxisConstruction axis
                    )
                 => axis
                 -> M.Map ItemTagId Color
                 -> [ItemTagId]
                 -> With axis            (SpaceOf style)
                 -> With (NextAxis axis) (SpaceOf style)
                 -> Confine axis (SpaceOf style)
                 -> Layout style
constructCorner axis colorMap layers parentCut parentLine tree =
        let stopCut   = tree ^. confineCut
            start     = pointFromAxis axis parentCut parentLine
            end       = pointFromAxis axis stopCut   parentLine
            pathLine :: Layout style
            pathLine  = withColor (transparent 0.2 black) . mask . stroke 1 . makeOpenCurve $ [line start end]
            cornerString = (show $ tree ^. confineCurveTag) ++{- "->" ++ (show $ tree ^. confineCrossedCurves) ++-} " ?> " ++ (show $ tree ^. confineConsidered) -- (show end)
            cornerText :: Layout style
            cornerText = translateBy end . rotateBy (15 @@ deg) . translateByXY 10 0 . scaleBy 20 . withColor blue . blurb $ cornerString
        in  overlap [ pathLine
                    --  constructLayerStack colorMap end layers
                    --, cornerText
                    --, rectangleAround end
                    ]



showBool True  = "1"
showBool False = "0"

showAxis (Left Horizontal) = showSymbol Horizontal
showAxis (Right Vertical)  = showSymbol Vertical

constructConfineTree :: forall style
                     .  (IsStyle style)
                     => M.Map ItemTagId Color
                     -> ConfineTree (SpaceOf style)
                     -> Layout style
constructConfineTree colorMap tree = go Vertical tree 0 (onAxis Vertical minBound) (onAxis Horizontal minBound) [] reasonableBoundaries
  where
  go :: (Axis axis, AxisConstruction axis, axis~NextAxis(NextAxis axis))
     => axis
     -> Branch axis (SpaceOf style)
     -> Int
     -> With axis            (SpaceOf style)
     -> With (NextAxis axis) (SpaceOf style)
     -> [ItemTagId]
     -> Box (SpaceOf style)
     -> Layout style
  go axis mTree depth parentCut parentLine layers boundary =
        if widthOf boundary > 0 && heightOf boundary > 0
        then case mTree of
               Nothing   -> emptyItem
               Just tree ->
                      let layers' = traverseCrossings (tree ^. confineCrossings) layers
                          confine = constructConfine axis colorMap layers' parentCut parentLine tree boundary
                          cut     = tree ^.  confineCut
                          (lessBound, moreBound) = splitBox axis (fromAxis axis cut) boundary
                          lessBranch = go (nextAxis axis) (tree ^.  confineLessCut) (depth + 1) parentLine cut layers' lessBound
                          moreBranch = go (nextAxis axis) (tree ^.  confineMoreCut) (depth + 1) parentLine cut layers' moreBound
                      in  overlap [confine, lessBranch, moreBranch]
        else emptyItem
