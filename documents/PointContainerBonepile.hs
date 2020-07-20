instance (PointContainer rep) => PointContainer (SRep token tex rep) where
   type ContainerFunctor (SRep token tex rep) = ContainerFunctor rep
   containedPoints = containedPoints . view sRep
   mapOverPoints f = over sRep (mapOverPoints f)


instance {-# OVERLAPPABLE #-} (HasSpace t, Space (SpaceOf t), PointContainer t) => SimpleTransformable t where
    translateBy p = mapOverPoints (translateBy p)
    scaleBy     s = mapOverPoints (scaleBy s)
    stretchBy   p = mapOverPoints (stretchBy p)
instance {-# OVERLAPPABLE #-} (HasSpace t, Space (SpaceOf t), PointContainer t) => Transformable t where
    rotateBy    a = mapOverPoints (rotateBy a)
