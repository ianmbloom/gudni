-- | A poorly designed data structure for representing various "plot" based shapes.
data Plot s where
  Plot        :: Curve s -> Plot s
  PlotJoin    :: Plot s -> Plot s -> Plot s
  PlotArc     :: s -> Angle s -> Plot s
  PlotSegment :: X s -> Plot s
  PlotRotate  :: Angle s -> Plot s -> Plot s
  PlotScale   :: s -> Plot s -> Plot s
  PlotFlipH   :: Plot s -> Plot s
  PlotFlipV   :: Plot s -> Plot s
  PlotReverse :: Plot s -> Plot s

deriving instance (Show s) => Show (Plot s)
deriving instance (Eq s) => Eq (Plot s)
deriving instance (Ord s) => Ord (Plot s)

instance NFData s => NFData (Plot s) where
  rnf = \case
    Plot        a    -> a `deepseq`             ()
    PlotJoin    a b  -> a `deepseq` b `deepseq` ()
    PlotArc     a b  -> a `deepseq`             ()
    PlotSegment a    -> a `deepseq`             ()
    PlotRotate  a b  -> a `deepseq` b `deepseq` ()
    PlotScale   a b  -> a `deepseq` b `deepseq` ()
    PlotFlipH   a    -> a `deepseq`             ()
    PlotFlipV   a    -> a `deepseq`             ()
    PlotReverse a    -> a `deepseq`             ()

instance Hashable s => Hashable (Plot s) where
  hashWithSalt s = \case
    Plot        a    -> s `hashWithSalt` (0::Int) `hashWithSalt` a
    PlotJoin    a b  -> s `hashWithSalt` (1::Int) `hashWithSalt` a `hashWithSalt` b
    PlotArc     a b  -> s `hashWithSalt` (2::Int) `hashWithSalt` a `hashWithSalt` b
    PlotSegment a    -> s `hashWithSalt` (3::Int) `hashWithSalt` a
    PlotRotate  a b  -> s `hashWithSalt` (6::Int) `hashWithSalt` a `hashWithSalt` b
    PlotScale   a b  -> s `hashWithSalt` (7::Int) `hashWithSalt` a `hashWithSalt` b
    PlotFlipH   a    -> s `hashWithSalt` (8::Int) `hashWithSalt` a
    PlotFlipV   a    -> s `hashWithSalt` (9::Int) `hashWithSalt` a
    PlotReverse a    -> s `hashWithSalt` (10::Int) `hashWithSalt` a

expandPlot :: (Show s, Floating s, Ord s) => Plot s -> [Segment s]
expandPlot = expandPlot'

-- all plots should start at (0,0)
expandPlot' :: (Show s, Floating s, Ord s) => Plot s -> [Segment s]
expandPlot' (Plot vs) = vs
expandPlot' (PlotJoin ap bp) = let as = expandPlot ap
                                   bs = expandPlot bp
                               in if null as
                                   then bs
                                   else if null bs
                                        then as
                                        else let end   = last as ^. stripVertex
                                                 start = head bs ^. stripVertex
                                                 f = overSegment ((end ^-^ start) ^+^)
                                                 bs' = map f bs
                                             in  as ++ tail bs'
expandPlot' (PlotArc r a) = let arc = expandPlot $ PlotScale r $ makeArcPlot a
                                start = head arc ^. onCurve
                            in  map (overSegment (^-^ start)) arc
expandPlot' (PlotSegment l)  = [Curved 0 0 l 0]
expandPlot' (PlotRotate a p)= map (overSegment (rotate a)) $ expandPlot p
expandPlot' (PlotScale s p) = map (overSegment (^* s)) $ expandPlot p
expandPlot' (PlotReverse p) = reverse $ expandPlot p
expandPlot' (PlotFlipH p) = map (\ (Seg o c) -> Seg o (makePoint (negate $ p ^. pX)  (p ^. pY))) (expandPlot p)
expandPlot' (PlotFlipV p) = map (\ (Seg o c) -> Seg o (makePoint (p ^. pX) (negate $ p ^. pY) )) (expandPlot p)
