{-# LANGUAGE TypeApplications #-}
module CriterionBenchmarks where

import           Graphics.Gudni.Figure.OpenCurve
import           Graphics.Gudni.Figure.Segment
import           Graphics.Gudni.Figure.Space

import qualified Graphics.Gudni.Figure.Bezier as BZ

import           Linear hiding (project)
import           Linear.Affine

import           Criterion
import           Criterion.Main

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
        let ε = 1e-6
        in nf (BZ.inverseArcLength @Float (BZ.maxStepsFromAccuracy ε) (Just ε)  bezier0) 0.5
    , bench "inverseArcLength, Float, ε = 1e-9" $
        let ε = 1e-9
        in nf (BZ.inverseArcLength @Float (BZ.maxStepsFromAccuracy ε) (Just ε)  bezier0) 0.5
    -- , bench "projectPoint, Float, ε = 1e-6" $
    --     let ε = 1e-6
    --     in nf (projectPoint (BZ.maxStepsFromAccuracy ε) (Just ε)  curve0) curve0
    , bench "projectWithStepsAccuracy, Float, ε = 1e-6" $
        let ε = 1e-6
        in nf (projectWithStepsAccuracy (BZ.maxStepsFromAccuracy ε) (Just ε) curve0) curve0
    , bench "projectWithAccuracy, Float, ε = 1e-6" $
      nf (projectWithAccuracy 1e-6  curve0) curve0
    , bench "projectWithSteps, Float" $
      nf (projectWithSteps (BZ.maxStepsFromAccuracy 1e-6) curve0) curve0
    , bench "project, Float" $
      nf (project curve0) curve0
    ]

bezier0 :: Fractional s => BZ.Bezier s
bezier0 = V3 (P (V2 0 0)) (P (V2 0.75 0.5)) (P (V2 1 0))

curve0 :: OpenCurve Float
curve0 = OpenCurve
    [Seg (P (V2 0 0)) (Just (P (V2 0.75 0.5))), Seg (P (V2 1 0)) (Just (P (V2 1.25 (-0.75))))]
    (P (V2 2 0))
