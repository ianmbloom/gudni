{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Experimental.ConstructConfineTree
  ( constructConfineTree
  , constructConfineTreeFromBoxes
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
import Text.PrettyPrint.GenericPretty

boxToRectangle :: Space s => Box s -> CompoundTree s
boxToRectangle box = mask . shapeFrom . translateBy (box ^. topLeftBox) . rectangle . sizeBox $ box

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

squareAround :: Space s => s -> s -> CompoundTree s
squareAround thickness size = translateBy (pure ((-size)/2)) . mask . strokeOffset 0 thickness . shapeFrom $ rectangle (pure size)

splitBox axis cutPoint box =
  if isVertical axis
  then (set rightSide  cutPoint box, set leftSide cutPoint box)
  else (set bottomSide cutPoint box, set topSide cutPoint box)


boxSectionsVertical cutBox box =
  let (top, rest)      = splitBox Horizontal (cutBox ^. topSide) box
      (middle, bottom) = splitBox Horizontal (cutBox ^. bottomSide) rest
  in  (top, middle, bottom)

boxSectionsHorizontal cutBox box =
  let (left, rest)    = splitBox Vertical (cutBox ^. leftSide ) box
      (middle, right) = splitBox Vertical (cutBox ^. rightSide) rest
  in  (left, middle, right)


trimCurve :: forall s . (Space s) => Box s -> Bezier s -> Maybe (Bezier s)
trimCurve box sourceBez = (trimVertical box >=> trimHorizontal box) sourceBez
    where
    bezBox = boxOf sourceBez
    trim :: (Space s)
         => Lens' (Box s) s
         -> Axis
         -> (s -> s -> Bool)
         -> ((Bezier s, Bezier s) -> Bezier s)
         -> Box s
         -> Bezier s
         -> Bezier s
    trim side axis comp f box bez =
        if (boxOf bez ^. side) `comp` (box ^. side)
        then f $ splitAtCut axis (box ^. side) bez
        else bez

    trimVertical :: Box s
                 -> Bezier s
                 -> Maybe (Bezier s)
    trimVertical box bez =
        if (bezBox ^. rightSide > box ^. leftSide) && (bezBox ^. leftSide < box ^. rightSide)
        then Just $ trim leftSide   Vertical   (<) snd box .
                    trim rightSide  Vertical   (>) fst box $ bez
        else Nothing

    trimHorizontal :: Box s
                   -> Bezier s
                   -> Maybe (Bezier s)
    trimHorizontal box bez =
        if (bezBox ^. bottomSide > box ^. topSide) && (bezBox ^. topSide < box ^. bottomSide)
        then Just $ trim topSide    Horizontal (<) snd box .
                    trim bottomSide Horizontal (>) fst box $ bez
        else Nothing

dec :: Maybe Int -> Maybe Int
dec x = subtract 1 <$> x

testDepth :: Maybe Int -> Bool
testDepth depth = isNothing depth || fromJust depth > 0

boxCenter box = ((box ^. topLeftBox) + (box ^. bottomRightBox)) / 2

data CurveState s = Complete Bool | Curve (Bezier s) deriving (Show)

data ShapeState s = ShS ConfineSubstanceId (CurveState s) deriving (Show)

constructConfineTree :: forall m token . (Monad m) => ConfineTree SubSpace -> FontMonad m (ShapeTree token SubSpace)
constructConfineTree tree = goV tree 0 reasonableBoundaries
  where
  thickness = 1
  goV :: BranchV SubSpace -> Int -> Box SubSpace -> FontMonad m (ShapeTree token SubSpace)
  goV mVTree depth boundary =
       if widthOf boundary > 0 && heightOf boundary > 0
       then case mVTree of
                Nothing -> return emptyItem
                Just vTree ->
                    let xCut        = vTree ^. confineBranch . confineXCut
                        xOverlap  = vTree ^. confineBranch . confineXOverlap
                        curveBox  = boxOf (vTree ^. confineCurve) :: Box SubSpace
                        (leftBound, rightBound) = splitBox Vertical xCut boundary
                        orientColor = red
                        vertLine    = withColor (transparent 0.2 orientColor) . mask . shapeFrom . stroke thickness . makeOpenCurve . pure $
                                      line (Point2 xCut (boundary ^. topSide + 4)) (Point2 xCut (boundary ^. bottomSide - 4))
                        -- vertical    = withColor (transparent 0.05 orientColor) . boxToRectangle $
                        --               makeBox xCut (boundary ^. topSide) xOverlap (boundary ^. bottomSide)
                        curve       = represent False (vTree ^. confineCurve)
                        cornerColor = transparent 0.5 $ if even (vTree ^. confineCornerWinding) then white else black
                    in
                    do leftBranch  <- goH (vTree ^. confineBranch . confineLeft ) (depth + 1) leftBound
                       rightBranch <- goH (vTree ^. confineBranch . confineRight) (depth + 1) rightBound
                       text <- (^?! unGlyph) <$> blurb 0.1 AlignMin (show $ vTree ^. confineCurveTag) :: FontMonad m (CompoundTree SubSpace)
                       let label = translateBy (boxCenter curveBox) .
                                   scaleBy 20 .
                                   withColor (transparent 0.5 (dark red)) $
                                   text
                       cornerText <- (^?! unGlyph) <$> blurb 0.1 AlignMin (show (vTree ^. confineCornerWinding)) :: FontMonad m (CompoundTree SubSpace)
                       let corner      = overlap [ withColor cornerColor . translateBy (curveBox ^. topLeftBox) . closedCircle $ 4
                                                 , withColor (transparent 0.5 orientColor) . translateBy (curveBox ^. topLeftBox) . closedCircle $ 6
                                                 , withColor cornerColor . translateBy (curveBox ^. topLeftBox) . translateByXY 3 (-7.5) . scaleBy 15 $ cornerText
                                                 ]
                       return $ overlap [label, corner, curve, vertLine, {-vertical,-} leftBranch, rightBranch]
       else return emptyItem
  goH :: BranchH SubSpace -> Int -> Box SubSpace -> FontMonad m (ShapeTree token SubSpace)
  goH mHTree depth boundary =
       if widthOf boundary > 0 && heightOf boundary > 0
       then case mHTree of
                Nothing -> return emptyItem
                Just hTree ->
                    let yCut      = hTree ^. confineBranch . confineYCut
                        yOverlap  = hTree ^. confineBranch . confineYOverlap
                        curveBox  = boxOf (hTree ^. confineCurve)
                        (topBound, bottomBound) = splitBox Horizontal yCut boundary
                        orientColor  = blue
                        horiLine     = withColor (transparent 0.5 orientColor) . mask . shapeFrom . stroke thickness . makeOpenCurve . pure $
                                       line (Point2 (boundary ^. leftSide + 4) yCut) (Point2 (boundary ^. rightSide - 4) yCut)
                        --horizontal   = withColor (transparent 0.05 orientColor) . boxToRectangle $
                        --               makeBox (boundary ^. leftSide) yCut (boundary ^. rightSide) yOverlap
                        curve        = represent False (hTree ^. confineCurve)
                        cornerColor = if even (hTree ^. confineCornerWinding) then white else black
                    in
                    do  topBranch    <- goV (hTree ^. confineBranch . confineTop   ) (depth + 1) topBound
                        bottomBranch <- goV (hTree ^. confineBranch . confineBottom) (depth + 1) bottomBound
                        text <- (^?! unGlyph) <$> blurb 0.1 AlignMin (show $ hTree ^. confineCurveTag) :: FontMonad m (CompoundTree SubSpace)
                        let label = translateBy (boxCenter curveBox) .
                                    scaleBy 20 .
                                    withColor (transparent 0.5 (dark blue)) $
                                    text
                        cornerText <- (^?! unGlyph) <$> blurb 0.1 AlignMin (show (hTree ^. confineCornerWinding)) :: FontMonad m (CompoundTree SubSpace)
                        let corner      = overlap [ withColor cornerColor . translateBy (curveBox ^. topLeftBox) . closedCircle $ 4
                                                  , withColor (transparent 0.5 orientColor) . translateBy (curveBox ^. topLeftBox) . closedCircle $ 6
                                                  , withColor cornerColor . translateBy (curveBox ^. topLeftBox) . translateByXY 3 (-7.5) . scaleBy 15 $ cornerText
                                                  ]
                        return $ overlap [label, corner, curve, horiLine, {-horizontal,-} topBranch, bottomBranch]
       else return emptyItem

confineTreeBoxes :: forall s . Space s => ConfineTree s -> [Box s]
confineTreeBoxes tree = goV tree reasonableBoundaries
  where
  goV :: BranchV s -> Box s -> [Box s]
  goV mVTree boundary =
       if widthOf boundary > 0 && heightOf boundary > 0
       then case mVTree of
                Nothing -> [boundary]
                Just vTree ->
                    let xCut        = vTree ^. confineBranch . confineXCut
                        (leftBound, rightBound) = splitBox Vertical xCut boundary
                        leftBranch  = goH (vTree ^. confineBranch . confineLeft ) leftBound
                        rightBranch = goH (vTree ^. confineBranch . confineRight) rightBound
                    in  leftBranch ++ rightBranch
       else [boundary]
  goH :: BranchH s -> Box s -> [Box s]
  goH mHTree boundary =
       if widthOf boundary > 0 && heightOf boundary > 0
       then case mHTree of
                Nothing -> [boundary]
                Just hTree ->
                    let yCut         = hTree ^. confineBranch . confineYCut
                        (topBound, bottomBound) = splitBox Horizontal yCut boundary
                        topBranch    = goV (hTree ^. confineBranch . confineTop   ) topBound
                        bottomBranch = goV (hTree ^. confineBranch . confineBottom) bottomBound
                    in  topBranch ++ bottomBranch
       else [boundary]

curvesInBox :: forall s . Space s => ConfineTree s -> Box s -> [Bezier s]
curvesInBox tree box = catMaybes . map (trimCurve box) . goV $ tree
    where
    goV :: BranchV s -> [Bezier s]
    goV mVTree =
        case mVTree of
            Nothing -> []
            Just vTree ->
                let xCut        = vTree ^. confineBranch . confineXCut
                    xOverlap    = vTree ^. confineBranch . confineXOverlap
                    curve       = vTree ^. confineCurve
                    leftBranch  = if xOverlap > box ^. leftSide  then         goH (vTree ^. confineBranch . confineLeft ) else []
                    rightBranch = if xCut     < box ^. rightSide then curve : goH (vTree ^. confineBranch . confineRight) else []
                in  leftBranch ++ rightBranch
    goH :: BranchH s -> [Bezier s]
    goH mHTree =
        case mHTree of
            Nothing -> []
            Just hTree ->
                let yCut         = hTree ^. confineBranch . confineYCut
                    yOverlap     = hTree ^. confineBranch . confineYOverlap
                    curve        = hTree ^. confineCurve
                    topBranch    = if yOverlap > box ^. topSide    then         goV (hTree ^. confineBranch . confineTop   ) else []
                    bottomBranch = if yCut     < box ^. bottomSide then curve : goV (hTree ^. confineBranch . confineBottom) else []
                in  topBranch ++ bottomBranch

drawCurve :: Space s => Bezier s -> ShapeTree token s
drawCurve bez =
  let s = bez ^. bzStart
      e = bez ^. bzEnd
      i = insideBoundaryPoint bez
  in  withColor (light green) . mask . shapeFrom $ makeOutline [line e i, line i s, bez]



constructConfineTreeFromBoxes :: forall s token . (Out s, Space s) => ConfineTree s -> ShapeTree token s
constructConfineTreeFromBoxes tree =
    overlap . map (drawCurve) . concatMap (curvesInBox tree) . confineTreeBoxes $ tree
