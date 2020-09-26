.....
eitherMaybeMeld :: (t -> a -> a -> a) -> t -> Maybe a -> Maybe a -> Maybe a
eitherMaybeMeld f a = eitherMaybe (f a)

execTransformerMaybeBox :: Space s => Transformer s -> Maybe (Box s) -> Maybe (Box s)
execTransformerMaybeBox transformer =
    case transformer of
        Simple trans         -> execSimpleTransformerMaybeBox trans
        Rotate angle         -> const Nothing
        Project path         -> const Nothing
        CombineTransform a b -> execTransformerMaybeBox b . execTransformerMaybeBox a

execSimpleTransformerMaybeBox simpleTransformer =
     case simpleTransformer of
        IdentityTransform    -> id
        Translate delta      -> fmap (mapBox (translate_ delta) )
        Stretch size         -> fmap (mapBox (stretch_ size)    )
        CombineSimple a b    -> execSimpleTransformerMaybeBox b . execSimpleTransformerMaybeBox a


instance (Space s) => CanApplyTransformer (Facet_ s) where
  applyTransformer f = over facetSides (fmap (applyTransformer f))

instance (Space s) => CanApplyTransformer (FacetSide s) where
  applyTransformer f = over facetOutput (applyTransformer f)

instance (Space s) => CanApplyTransformer (Bezier s) where
  applyTransformer f = execTransformer f

execTransformer :: {-(CanApplyProjection t, PointContainer t) => -} Transformer (SpaceOf t) -> t -> t
execTransformer transformer = undefined
    case transformer of
        Simple trans         -> execSimpleTransformer trans
        Rotate angle         -> rotation angle
        Project path         -> projectDefault False . makeBezierSpace arcLength $ path
        CombineTransform a b -> execTransformer b . execTransformer a

fromSRep :: SRep a b (Maybe c) -> Maybe (SRep a b c)
fromSRep (SRep a b mC) = fmap (SRep a b) mC

fullCompoundTree :: CompoundTree s -> Maybe (FullCompoundTree s)
fullCompoundTree = fullSTree

fullSRep :: SRep a b (CompoundTree s) -> SRep a b (Maybe (FullCompoundTree s))
fullSRep (SRep a b tree) = SRep a b (fullCompoundTree tree)

mergeSRep :: Maybe (SRep a b (CompoundTree s)) -> Maybe (SRep a b (FullCompoundTree s))
mergeSRep = join . fmap (fromSRep . fullSRep)

fullMerge :: ( Leaf a~SBranch a
             ) => STree (MaybeItem a) -> STree a
fullMerge = mapSItem mergeSRep

fullShapeTree :: forall a b token tex s g
              .  ( Leaf a~SBranch a
                 , Leaf b~SBranch b
                 , Item a~Maybe (SRep token tex (CompoundTree s))
                 , Tag a~Tag b
                 , Meld a~Meld b
                 )
              => STree a
              -> Maybe (STree b)
fullShapeTree tree = fullSTree . fullMerge $ tree
