data HardFacet_ s = HardFacet
  { _hardFace    :: V3 (Point2 s)
  , _hardTexture :: V3 (Point2 s)
  } deriving (Show)
makeLenses ''HardFacet_

type HardFacet = HardFacet_ SubSpace

hardenFacet :: Facet_ s -> HardFacet_ s
hardenFacet facet =
    let sceneFacet = fmap (view _x . stripBezier . view facetOutput) . view facetSides $ facet
        textureFacet = fmap (view (facetInput . _x)) . view facetSides $ facet
    in  HardFacet { _hardFace = sceneFacet, _hardTexture = textureFacet}

instance (Space s) => HasSpace (HardFacet_ s) where
    type SpaceOf (HardFacet_ s) = s

instance (Space s) => CanBox (HardFacet_ s) where
    boxOf (HardFacet face _) = foldl1 minMaxBox (fmap boxOf face)

instance StorableM HardFacet where
    sizeOfM _ = do sizeOfM (undefined :: V3 (Point2 SubSpace))
                   sizeOfM (undefined :: V3 (Point2 TextureSpace))
    alignmentM _ = do alignmentM (undefined :: V3 (Point2 SubSpace))
                      alignmentM (undefined :: V3 (Point2 TextureSpace))
    peekM = do scene   <- peekM
               texture <- peekM
               return (HardFacet scene texture)
    pokeM (HardFacet scene texture) = do pokeM scene
                                         pokeM texture

instance Storable HardFacet where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance (NFData s) => NFData (HardFacet_ s) where
  rnf (HardFacet face tex) = face `deepseq` tex `deepseq` ()


sideTriangle :: (Space s, Out s)
             => Lens' (V3 s) s
             -> Lens' (V3 s) s
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> Lens' (V3 (FacetSide s)) (FacetSide s)
             -> V3 s
             -> Facet_ s
             -> Facet_ s
sideTriangle leftSplit rightSplit left right bottom splitPoints facet =
  let (_, left')  = splitBezierSide (splitPoints ^. leftSplit)  (facet ^. facetSides . left)
      (right', _) = splitBezierSide (splitPoints ^. rightSplit) (facet ^. facetSides . right)
      midControl = mid (facet ^. facetSides . left . facetOutput . bzEnd) (facet ^. facetSides . bottom . facetOutput . bzControl)
      bottomBez = Bez (right' ^. facetOutput . bzEnd) midControl (left' ^. facetOutput . bzStart)
      bottomTex = V2 (left' ^. facetInput . _x) (right' ^. facetInput . _y)
      bottom' = FacetSide bottomBez bottomTex
  in  set (facetSides . left) left' . set (facetSides . right) right' . set (facetSides . bottom) bottom' $ facet

reverseFacetSide :: FacetSide s -> FacetSide s
reverseFacetSide (FacetSide scene (V2 t0 t1)) = FacetSide (reverseBezier scene) (V2 t1 t0)

subdivideFacetT :: (Space s, Out s) => V3 s -> Facet_ s -> V4 (Facet_ s)
subdivideFacetT splitPoints facet =
  let triZ  = sideTriangle _x _y   _x _y _z splitPoints facet
      triX  = sideTriangle _y _z   _y _z _x splitPoints facet
      triY  = sideTriangle _z _x   _z _x _y splitPoints facet
      triIn = Facet {_facetSides = fmap reverseFacetSide $
                                   V3 (triZ ^. facetSides . _z)
                                      (triX ^. facetSides . _x)
                                      (triY ^. facetSides . _y)
                    }
  in  V4 triX triY triZ triIn

subdivideFacet :: (Space s, Alternative f, Out s) => Facet_ s -> f (Facet_ s)
subdivideFacet = foldl1 (<|>) . fmap pure . subdivideFacetT (pure hALF)

splitLineSegment :: (Space s) => s -> V2 (Point2 s) -> (V2 (Point2 s), V2 (Point2 s))
splitLineSegment t (V2 v0 v1) = let midPoint = lerp t v0 v1 in (V2 v0 midPoint, V2 midPoint v1)

splitBezierSide :: (Space s) => s -> FacetSide s -> (FacetSide s, FacetSide s)
splitBezierSide t (FacetSide bez tex) =
  let (leftBez, rightBez) = splitBezier t bez
      (leftTex, rightTex) = splitLineSegment t tex
  in  (FacetSide leftBez leftTex, FacetSide rightBez rightTex)

maybeSubdivideFacet :: (Space s, Alternative f, Out s) => (Bezier s -> Maybe s) -> Facet_ s -> Maybe (f (Facet_ s))
maybeSubdivideFacet f facet =
    let mSplitPoints = fmap (f . view facetOutput) $ facet ^. facetSides
    in  if or (fmap isJust mSplitPoints)
        then let splitPoints = fmap (fromMaybe hALF) mSplitPoints
                 (V4 triZ triX triY triIn) = subdivideFacetT splitPoints facet
             in  Just $ pure triX <|> pure triY <|> pure triZ <|> pure triIn
        else Nothing

maybeCutFacet :: (Space s, Out s) => (Bezier s -> Maybe s) -> Facet_ s -> Maybe (FacetGroup s, FacetGroup s)
maybeCutFacet f facet =
    let mSplitPoints = fmap (f . view facetOutput) $ facet ^. facetSides
    in  if or (fmap isJust mSplitPoints)
        then let splitPoints = fmap (fromMaybe hALF) mSplitPoints
                 (V4 triX triY triZ triIn) = subdivideFacetT splitPoints facet
             in  case mSplitPoints of
                    V3 Nothing  (Just _) (Just _) -> Just (FacetGroup $ pure triX, FacetGroup $ pure triY <|> pure triZ <|> pure triIn)
                    V3 (Just _) Nothing  (Just _) -> Just (FacetGroup $ pure triY, FacetGroup $ pure triX <|> pure triZ <|> pure triIn)
                    V3 (Just _) (Just _) Nothing  -> Just (FacetGroup $ pure triZ, FacetGroup $ pure triX <|> pure triY <|> pure triIn)
                    _ -> error "split not quite working"
        else Nothing

instance (Space s, Out s) => CanDeknob (Facet_ s) where
    deKnob axis = maybeSubdivideFacet (maybeKnobSplitPoint axis)

instance (Space s, Out s) => CanCut (FacetGroup s) where
    -- | Split item across horizontal or vertical line
    splitAtCut axis splitPoint = undefined . fmap (maybeCutFacet (maybeCutPointBezier axis splitPoint)) . unFacetGroup
    -- | Determine if horizontal or vertical line cuts item
    canCut axis splitPoint = undefined -- or . join . fmap (fmap (canCut axis splitPoint . view facetOutput) . view facetSides) . unFacetGroup

stripBezier :: Bezier s -> V2 (Point2 s)
stripBezier (Bez v0 c v1) = V2 v0 v1

linesToFacetSide :: Space s => Point2 s -> Point2 s -> Point2 s -> Point2 s -> FacetSide s
linesToFacetSide p0 p1 t0 t1 = FacetSide (line p0 p1) (V2 t0 t1)


newtype FacetGroup_ f s = FacetGroup {unFacetGroup :: f (Facet_ s)} deriving (Generic)

type FacetGroup s = FacetGroup_ ShapeFunctor s

instance (Out (Facet_ s)) => Out (FacetGroup_ ShapeFunctor s) where
    doc = doc . V.toList . unFacetGroup
    docPrec _ = doc

instance Space s => HasSpace (FacetGroup_ f s) where
  type SpaceOf (FacetGroup_ f s) = s


instance ( Space s
         , Out s
         , Chain f
         ) => CanApplyProjection (FacetGroup_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace (FacetGroup facets) =
        let fixed :: f (Facet_ s)
            fixed = join . fmap (undefined {- replaceKnob verticalAxis-}) $ facets
        in  undefined -- join . fmap (traverseBezierSpace debug max_steps m_accuracy bSpace) $ FacetGroup fixed

facetStartPoints :: Facet_ s -> V3 (Point2 s)
facetStartPoints =  . view facetOutput

facetControls :: Facet_ s -> V3 (Point2 s)
facetControls  = fmap (view _y) . view facetOutput

facetEndPoints :: Facet_ s -> V3 (Point2 s)
facetEndPoints = fmap (view _x) . rotateTri1 . view facetOutput

facetToBeziers :: Facet_ s -> V3 (Bezier s)
facetToBeziers facet = bezTriToBeziers
  let starts   = facetStartPoints facet
      controls = facetControls    facet
      ends     = facetEndPoints   facet
  in  liftA3 Bez starts controls ends
