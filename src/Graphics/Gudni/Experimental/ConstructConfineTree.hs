{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Experimental.ConstructConfineTree
  ( constructConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Data.Maybe


boxToRectangle :: Space s => Box s -> CompoundTree s
boxToRectangle box = mask . shapeFrom . translateBy (box ^. topLeftBox) . rectangle . sizeBox $ box

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 16
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

squareAround :: Space s => s -> s -> CompoundTree s
squareAround thickness size = translateBy (pure ((-size)/2)) . mask . strokeOffset 0 thickness . shapeFrom $ rectangle (pure size)

constructBranch :: forall s . Space s => Bool -> Int -> Box s -> [Box s] -> ConfineTree s -> ShapeTree Int s
constructBranch fromIn depth boundaries subtractors branch =
  let p = branch ^. confinePosition
      tack = branch ^. confineTack
      horizontalBound = if tack ^. tackLeft
                        then leftSide
                        else rightSide
      verticalBound = if tack ^. tackAbove
                      then topSide
                      else bottomSide
      thickness = (1 / (1 + fromIntegral depth)) * 10
      horizontal :: CompoundTree s
      horizontal = mask . shapeFrom . stroke thickness . makeOpenCurve . pure $ line p (Point2 (boundaries ^. horizontalBound) (p ^. pY))
      vertical :: CompoundTree s
      vertical   = mask . shapeFrom . stroke thickness . makeOpenCurve . pure $ line p (Point2 (p ^. pX) (boundaries ^. verticalBound))
      subtractRectangles = overlap . map boxToRectangle $ subtractors
      confineAxis        = overlap [horizontal, vertical]
      axisColor = if fromIn then red else blue
      outP = outsideBoundaryPoint (branch ^. confineCurve)
  in  overlap [ withColor green  $ translateBy p    $ openCircle thickness
              , withColor purple $ translateBy outP $ openCircle thickness
              , withColor (transparent 0.5 axisColor) $ subtractFrom subtractRectangles confineAxis
              , represent False . view confineCurve $ branch
              ]

confineBoundaries :: ConfineTree s -> Box s -> Box s
confineBoundaries branch boundaries =
  let p = branch ^. confinePosition
      tack = branch ^. confineTack
      horizontalBound = if tack ^. tackLeft
                        then rightSide
                        else leftSide
      verticalBound = if tack ^. tackAbove
                      then bottomSide
                      else topSide
  in  set horizontalBound (p ^. pX) .
      set verticalBound   (p ^. pY) $
      boundaries

constructConfineStack :: Space s => [ConfineTree s] -> ShapeTree Int s
constructConfineStack = --overlap . map (represent False . view confineCurve)
                        const emptyItem

constructConfineTree :: forall s . Space s => Int -> Maybe (ConfineTree s) -> ShapeTree Int s
constructConfineTree step mTree = go False reasonableBoundaries [] [] mTree 0
    where
    go :: Space s => Bool -> Box s -> [Box s] -> [ConfineTree s] -> Maybe (ConfineTree s) -> Int -> ShapeTree Int s
    go fromIn boundaries subtractors stack mTree depth =
      if depth < step
      then
           case mTree of
             Nothing -> constructConfineStack stack
             Just branch ->
                let innerStack = branch:stack
                    insideBound = confineBoundaries branch boundaries
                    inside  = go True  insideBound subtractors               innerStack (branch ^. confineInside ) (depth + 1)
                    outside = go False boundaries  (insideBound:subtractors) stack      (branch ^. confineOutside) (depth + 1)
                in
                overlap $
                    [ constructBranch fromIn depth boundaries subtractors branch
                    , inside
                    , outside
                    ]
      else constructConfineStack stack
