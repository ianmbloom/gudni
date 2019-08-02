{-# LANGUAGE TypeApplications #-}
module CriterionBenchmarks where

import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Segment

import qualified Graphics.Gudni.Figure.Bezier as BZ

import Linear
import Linear.Affine

import Criterion
import Criterion.Main

main = defaultMain
    [ bench "length of single Bézier segment, analytical" $
        nf (BZ.arcLength @Float) bezier0
    , bench "length of single Bézier segment, analytical, Double" $
        nf (BZ.arcLength @Double) bezier0
    , bench "length of single Bézier segment, Legendre-Gauss" $
        nf (BZ.arcLength  @Float) (V3 (P (V2 0 0)) (P (V2 0.5 0.001)) (P (V2 1 0)))
    , bench "length of OpenCurve with straight line" $
        nf arcLength (OpenCurve [Seg (P (V2 0 0)) Nothing] (P (V2 1 (0 :: Float))))
    , bench "length of OpenCurve with single Bézier segment" $
        nf arcLength (OpenCurve [Seg (P (V2 0 0)) (Just (P (V2 0.5 0.5)))] (P (V2 1 (0 :: Float))))
    , bench "length of OpenCurve with two segments" $
        nf arcLength (OpenCurve
                         [ Seg (P (V2 0 (0 :: Float))) (Just (P (V2 0.5 0.5)))
                         , Seg (P (V2 1 0)) (Just (P (V2 1.5 (-0.5)))) ]
                         (P (V2 2 0)))
    , bench "inverseArcLength, Float, ε = 1e-6" $
        nf (BZ.inverseArcLength @Float 1e-6 bezier0) 0.5
    , bench "inverseArcLength, Float, ε = 1e-9" $
        nf (BZ.inverseArcLength @Float 1e-9 bezier0) 0.5
    ]

bezier0 :: Fractional s => BZ.Bezier s
bezier0 = V3 (P (V2 0 0)) (P (V2 0.5 0.5)) (P (V2 1 0))
