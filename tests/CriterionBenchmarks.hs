module CriterionBenchmarks where

import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Segment

import Linear
import Linear.Affine

import Criterion
import Criterion.Main

main = defaultMain
    [
    --  bench "length of single Bézier segment, analytical" $
    --    nf (arcLength (P (V2 0 0)) (P (V2 0.5 0.5))) (P (V2 1 (0 :: Float)))
    --, bench "length of single Bézier segment, analytical, Double" $
    --    nf (arcLength (P (V2 0 0)) (P (V2 0.5 0.5))) (P (V2 1 (0 :: Double)))
    --, bench "length of single Bézier segment, Legendre-Gauss" $
    --    nf (arcLength (P (V2 0 0)) (P (V2 0.5 0.001))) (P (V2 1 (0 :: Float)))
    --, bench "length of OpenCurve with straight line" $
    --    nf arcLength (fromSegments [straigh 0 0)) Nothing] (P (V2 1 (0 :: Float))))
    --, bench "length of OpenCurve with single Bézier segment" $
    --    nf arcLength (fromSegments [Seg (P (V2 0 0)) (Just (P (V2 0.5 0.5)))] (P (V2 1 (0 :: Float))))
    --, bench "length of OpenCurve with two segments" $
    --    nf arcLength (fromSegments
    --                     [ Seg (P (V2 0 (0 :: Float))) (Just (P (V2 0.5 0.5)))
    --                     , Seg (P (V2 1 0)) (Just (P (V2 1.5 (-0.5)))) ]
    --                     (P (V2 2 0)))
    ]
