{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}


module Graphics.Gudni.Experimental.ConstructConfineTree
  ( constructConfineTree
  , constructConfine
  , reasonableBoundaries
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Layout
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Experimental.ConstructConfineQuery
import Graphics.Gudni.Util.Representation
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

constructPath start end =
  overlap [ withColor white . mask . stroke 1 . makeOpenCurve $ [line start (interimPoint start end)]
          , withColor white . mask . stroke 1 . makeOpenCurve $ [line (interimPoint start end) end]
          ]

constructConfine :: forall axis style
                 .  ( IsStyle style
                    , AxisConstruction axis
                    )
                 => axis
                 -> M.Map ItemTagId Color
                 -> [ItemTagId]
                 -> Point2 (SpaceOf style)
                 -> Confine axis (SpaceOf style)
                 -> Box (SpaceOf style)
                 -> Layout style
constructConfine axis colorMap layers start tree boundary =
    let thickness :: SpaceOf style
        thickness = 1
        cut       = tree ^.  confineCut
        overhang  = tree ^.  confineOverhang
        anchorPoint = confineAnchorPoint tree
        aColor = axisColor axis
        aLine :: Bezier (SpaceOf style)
        aLine = axisLine axis cut boundary
        overhangBox :: Box (SpaceOf style)
        overhangBox = overlapBlock axis cut overhang boundary
        anchorPath = constructPath start anchorPoint
        axisShape :: Layout style
        axisShape = withColor (transparent 0.2 aColor) . mask . stroke thickness . makeOpenCurve $ [aLine]
        overhangShape :: Layout style
        overhangShape = withColor (transparent 0.1 aColor) . mask . boxToRectangle $ overhangBox
        curve         = bezierArrow $ (tree ^. confineCurve)
        text = blurb (show $ tree ^. confineCurveTag)
        label :: Layout style
        label = translateBy (eval 0.5 (tree ^. confineCurve)) .
                scaleBy 40 .
                withColor (transparent 0.5 purple) $
                text
        -- let cornerString = show (tree ^. confineCurveTag) ++ " " ++ show (confineAnchorPoint tree) ++ ":\n" ++
        --                    show (tree ^. confineCornerWinding)
        --                    ++ "\n" ++ intercalate "\n" (map show (tree ^. confineCornerHistory))
        -- cornerText = paragraph cornerString :: FontMonad m (CompoundTree s)
        corner = constructLayerStack colorMap anchorPoint layers
                     -- overlap [ withColor (transparent 0.5 cornerColor) . translateBy anchorPoint . closedCircle $ 4
                     --        , withColor (transparent 0.5 aColor) . translateBy anchorPoint . closedCircle $ 6
                     --        --, withColor cornerColor . translateBy anchorPoint . translateByXY 6 (-7.5) . scaleBy 15 $ cornerText
                     --        ]
    in  overlap $ [label, corner, curve, axisShape, anchorPath{-, overhangShape-}]

constructConfineTree :: forall style
                     .  (IsStyle style)
                     => M.Map ItemTagId Color
                     -> ConfineTree (SpaceOf style)
                     -> Layout style
constructConfineTree colorMap tree = go Vertical tree 0 outsidePoint [] reasonableBoundaries
  where
  go :: (Axis axis, AxisConstruction axis)
     => axis
     -> Branch axis (SpaceOf style)
     -> Int
     -> Point2 (SpaceOf style)
     -> [ItemTagId]
     -> Box (SpaceOf style)
     -> Layout style
  go axis mTree depth start layers boundary =
        if widthOf boundary > 0 && heightOf boundary > 0
        then case mTree of
                 Nothing -> emptyItem
                 Just tree ->
                        let layers' = traverseCrossings (tree ^. confineCrossings) layers
                            confine = constructConfine axis colorMap layers' start tree boundary
                            cut     = tree ^.  confineCut
                            anchor  = confineAnchorPoint tree
                            (lessBound, moreBound) = splitBox axis cut boundary
                            lessBranch = go (nextAxis axis) (tree ^.  confineLessCut) (depth + 1) anchor layers' lessBound
                            moreBranch = go (nextAxis axis) (tree ^.  confineMoreCut) (depth + 1) anchor layers' moreBound
                        in  overlap [confine, lessBranch, moreBranch]
        else emptyItem
