+newTag: 0 treeCurveTag: 3   pLine V0.00000 start: H(minBound) end: H0.00000 False
+newTag: 0 treeCurveTag: 4   pLine H0.00000 start: V0.00000 end: V(-300.00000) True
consTag 0 treeTag 4 [0]
+newTag: 0 treeCurveTag: 1   pLine V0.00000 start: H(minBound) end: H0.00000 False
+newTag: 0 treeCurveTag: 5   pLine H0.00000 start: V0.00000 end: V0.00000 False
+newTag: 0 treeCurveTag: 2   pLine H0.00000 start: V0.00000 end: V0.00000 False

decorateConfineTree Just Confine {_confineItemTagId = 0,
              _confineCrossings = [],
              _confineCrossedCurves = [],
              _confineCurveTag = 0,
              _confineCurve = Bezier {unBezier = V3 (0.0 , 0.0)
                                                    (0.0 , 150.0)
                                                    (0.0 , 300.0)},
              _confineCut = V0.00000,
              _confineOverhang = V0.00000,
              _confineLessCut = Just Confine {_confineItemTagId = 1,
                                              _confineCrossings = [0],
                                              _confineCrossedCurves = [3],
                                              _confineCurveTag = 3,
                                              _confineCurve = Bezier {unBezier = V3 (0.0 , 0.0)
                                                                                    (-150.0 , 0.0)
                                                                                    (-300.0 , 0.0)},
                                              _confineCut = H0.00000,
                                              _confineOverhang = H0.00000,
                                              _confineLessCut = Just Confine {_confineItemTagId = 1,
                                                                              _confineCrossings = [0],
                                                                              _confineCrossedCurves = [4],
                                                                              _confineCurveTag = 4,
                                                                              _confineCurve = Bezier {unBezier = V3 (-300.0 , 0.0)
                                                                                                                    (-150.0 , -150.0)
                                                                                                                    (0.0 , -300.0)},
                                                                              _confineCut = V(-300.00000),
                                                                              _confineOverhang = V(-300.00000),
                                                                              _confineLessCut = Nothing,
                                                                              _confineMoreCut = Nothing},
                                              _confineMoreCut = Nothing},
              _confineMoreCut = Just Confine {_confineItemTagId = 0,
                                              _confineCrossings = [0],
                                              _confineCrossedCurves = [1],
                                              _confineCurveTag = 1,
                                              _confineCurve = Bezier {unBezier = V3 (0.0 , 300.0)
                                                                                    (150.0 , 150.0)
                                                                                    (300.0 , 0.0)},
                                              _confineCut = H0.00000,
                                              _confineOverhang = H0.00000,
                                              _confineLessCut = Just Confine {_confineItemTagId = 1,
                                                                              _confineCrossings = [],
                                                                              _confineCrossedCurves = [],
                                                                              _confineCurveTag = 5,
                                                                              _confineCurve = Bezier {unBezier = V3 (0.0 , -300.0)
                                                                                                                    (0.0 , -150.0)
                                                                                                                    (0.0 , 0.0)},
                                                                              _confineCut = V0.00000,
                                                                              _confineOverhang = V0.00000,
                                                                              _confineLessCut = Nothing,
                                                                              _confineMoreCut = Nothing},
                                              _confineMoreCut = Just Confine {_confineItemTagId = 0,
                                                                              _confineCrossings = [],
                                                                              _confineCrossedCurves = [],
                                                                              _confineCurveTag = 2,
                                                                              _confineCurve = Bezier {unBezier = V3 (300.0 , 0.0)
                                                                                                                    (150.0 , 0.0)
                                                                                                                    (0.0 , 0.0)},
                                                                              _confineCut = V0.00000,
                                                                              _confineOverhang = V0.00000,
                                                                              _confineLessCut = Nothing,
                                                                              _confineMoreCut = Nothing}}}
