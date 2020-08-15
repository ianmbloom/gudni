
MaybeBox(..)
, maybeItem
, maybeBox


data MaybeBox a
   = MaybeBox { _maybeItem :: a
              , _maybeBox  :: Maybe (Box (SpaceOf a))
              }
makeLenses ''MaybeBox

instance HasSpace a => HasSpace (MaybeBox a) where
  type SpaceOf (MaybeBox a) = SpaceOf a

withBoxToMaybeBox :: (CanApplySimpleTransformer a) => WithBox a -> MaybeBox a
withBoxToMaybeBox (WithBox item box) = MaybeBox (applySimpleTransformer item) (Just box)

maybeBoxToWithBox :: CanBox a => MaybeBox a -> WithBox a
maybeBoxToWithBox (MaybeBox item mBox) =
  let box = case mBox of
                Nothing -> boxOf item
                Just b -> b
  in WithBox item IdentityTransform box

withBoxToMaybeBox :: (CanApplySimpleTransformer a) => WithBox a -> MaybeBox a
withBoxToMaybeBox (WithBox item box) = MaybeBox (applySimpleTransformer item) (Just box)

maybeBoxToWithBox :: CanBox a => MaybeBox a -> WithBox a
maybeBoxToWithBox (MaybeBox item mBox) =
  let box = case mBox of
                Nothing -> boxOf item
                Just b -> b
  in WithBox item IdentityTransform box

instance CanBox a => CanBox (MaybeBox a) where
  boxOf (MaybeBox item mBox) =
      case mBox of
        Just box -> box
        Nothing  -> boxOf item

instance (CanApplyProjection a) => CanApplyProjection (MaybeBox a) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path (MaybeBox item _) =
        MaybeBox (projectionWithStepsAccuracy debug max_steps m_accuracy path item) Nothing

instance (CanBox a, PointContainer a, CanApplyTransformer a) => CanApplyTransformer (MaybeBox a) where
    applyTransformer trans (MaybeBox item box) = MaybeBox (execTransformer trans item) (execTransformerMaybeBox trans box)

onLayoutMeld :: forall style meld item
             .  ( SpaceOf style~SpaceOf item
                , IsStyle style
                , CanBox item
                , CanApplyProjection item
                , PointContainer item
                , CanApplyTransformer item
                )
             => ProximityMeld style meld
             -> CollapseTree meld item
             -> CollapseTree meld item
             -> CollapseTree meld item
onLayoutMeld (ProximityMeld style proximity meld) a b =
    uncurry (SMeld meld) $
    let aC :: WithBox (Tree meld leaf)
        aC = collapseMaybeBox a
        bC :: WithBox (Tree meld leaf)
        bC = collapseMaybeBox b
        (aDelta, bDelta) = applyProximity style proximity (aC ^. withBox, bC ^. withBox)
    in ( translateBy aDelta . SLeaf . SItem . withBoxToMaybeBox $ aC
       , translateBy bDelta . SLeaf . SItem . withBoxToMaybeBox $ bC
       )
