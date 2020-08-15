

instance (SimpleTransformable rep) => SimpleTransformable (SRep token texture rep) where
    translateBy p = over sRep (translateBy p)
    scaleBy     s = over sRep (scaleBy s)
    stretchBy   p = over sRep (stretchBy p)
instance (Transformable rep) => Transformable (SRep token texture rep) where
    rotateBy    a = over sRep (rotateBy a)

instance Space s => SimpleTransformable (Bezier s) where
    translateBy delta = over bzPoints (fmap (translateBy delta))
    scaleBy     scale = over bzPoints (fmap (scaleBy scale))
    stretchBy   delta = over bzPoints (fmap (stretchBy delta))

instance Space s => Transformable (Bezier s) where
    rotateBy    angle = over bzPoints (fmap (rotateBy angle))

instance (Chain f, Space s, CanApplyProjection (BezierSpace s) t, Chain f) => CanApplyProjection (OpenCurve_ f s) t where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
      let bSpace = makeBezierSpace arcLength (view curveSegments path)
      in  projectionWithStepsAccuracy debug max_steps m_accuracy bSpace

-- | Connect two curves end to end by translating c1 so that the starting point of 'c1' is equal to the terminator of 'c0'

{-
instance ( Space s
         , Chain f
         )
         => CanApplyProjection (BezierSpace s) (Outline_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace curve =
         Outline . join . fmap (projectBezierWithStepsAccuracy debug max_steps m_accuracy bSpace) . view outlineSegments $ curve
-}

{-
-- Boxes have an instance for SimpleTransformable but no instance for Transformable.
instance Space s => SimpleTransformable (Box s) where
  translateBy p = mapBox (^+^ p)
  stretchBy   p = mapBox (liftA2 (*) p)
  scaleBy     s = mapBox (^* s)
-}

strokeBezierSpace offset thickness bSpace =
  let lengths = bezierSpaceLengths bSpace
      rect = segmentedRectangle thickness lengths
  in  projectDefault False bSpace . translateByXY 0 offset $ rect
