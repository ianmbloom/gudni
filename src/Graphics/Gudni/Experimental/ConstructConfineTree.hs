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
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty

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
  axisColor Vertical     = red
  axisLine  Vertical    cut boundary = line (Point2 cut (boundary ^. topSide + 4)) (Point2 cut (boundary ^. bottomSide - 4))
  overlapBlock Vertical cut overlap boundary = makeBox cut (boundary ^. topSide) overlap (boundary ^. bottomSide)

instance AxisConstruction Horizontal where
  axisColor Horizontal   = blue
  axisLine Horizontal     cut boundary = line (Point2 (boundary ^. leftSide + 4) cut) (Point2 (boundary ^. rightSide - 4) cut)
  overlapBlock Horizontal cut overlap boundary = makeBox (boundary ^. leftSide) cut (boundary ^. rightSide) overlap

bezierArrow :: (HasDefault token, Space s) => Bezier s -> ShapeTree token s
bezierArrow bz@(Bez v0 c v1) =
     let r = 5
         w = 10
         h = 7.5
     in
     overlap [ withColor black $ mask . withArrowHead (Point2 w h) PointingForward $ bz
             --, withColor red   $ translateBy v0 $ closedCircle r
             --, withColor (light blue)  $ translateBy  c $ openCircle r
             --, withColor red   $ translateBy v1 $ closedCircle r
             , withColor black $ mask . stroke 1 $ bz
             ]


constructConfine :: forall axis token s style . (HasDefault token, Space s, IsStyle style, AxisConstruction axis) => axis -> Confine axis s -> Box s -> Layout token s style
constructConfine axis tree boundary =
    let thickness :: s
        thickness = 1
        cut       = tree ^.  confineCut
        overhang  = tree ^.  confineOverhang
        anchorPoint = confineAnchorPoint tree
        aColor = axisColor axis
        aLine :: Bezier s
        aLine = axisLine axis cut boundary
        overhangBox :: Box s
        overhangBox = overlapBlock axis cut overhang boundary
        axisShape   = place . withColor (transparent 0.2 aColor) . mask . stroke thickness . makeOpenCurve $ [aLine]
        overhangShape :: Layout token s style
        overhangShape = place . withColor (transparent 0.1 aColor) . mask . boxToRectangle $ overhangBox
        curve       = place . bezierArrow $ (tree ^. confineCurve)
        --cornerColor = if even (tree ^. confineCornerWinding) then white else black
    in
    do let text = blurb (show $ tree ^. confineCurveTag)
           label :: Layout token s style
           label = translateBy (eval 0.5 (tree ^. confineCurve)) .
                   scaleBy 40 .
                   withColor (transparent 0.5 purple) $
                   text
       -- let cornerString = show (tree ^. confineCurveTag) ++ " " ++ show (confineAnchorPoint tree) ++ ":\n" ++
       --                    show (tree ^. confineCornerWinding)
       --                    ++ "\n" ++ intercalate "\n" (map show (tree ^. confineCornerHistory))
       -- cornerText <- fromGlyph <$> paragraph 0.1 0.1 AlignMin AlignMin cornerString :: FontMonad m (CompoundTree s)
       -- let corner = overlap [ withColor (transparent 0.5 cornerColor) . translateBy anchorPoint . closedCircle $ 4
       --                      , withColor (transparent 0.5 aColor) . translateBy anchorPoint . closedCircle $ 6
       --                      --, withColor cornerColor . translateBy anchorPoint . translateByXY 6 (-7.5) . scaleBy 15 $ cornerText
       --                      ]
       overlap $ [label, {-corner,-} curve, axisShape{-, overhangShape-}]

constructConfineTree :: forall token s style . (HasDefault token, Space s, IsStyle style) => ConfineTree s -> Layout token s style
constructConfineTree tree = go Vertical tree 0 reasonableBoundaries
  where
  go :: (Axis axis, AxisConstruction axis) => axis -> Branch axis s -> Int -> Box s -> Layout token s style
  go axis mTree depth boundary =
        if widthOf boundary > 0 && heightOf boundary > 0
        then case mTree of
                 Nothing -> emptyItem
                 Just tree ->
                        let confine = constructConfine axis tree boundary
                            cut     = tree ^.  confineCut
                            (lessBound, moreBound) = splitBox axis cut boundary
                            lessBranch = go (nextAxis axis) (tree ^.  confineLessCut) (depth + 1) lessBound
                            moreBranch = go (nextAxis axis) (tree ^.  confineMoreCut) (depth + 1) moreBound
                        in  overlap [confine, lessBranch, moreBranch]
        else emptyItem
