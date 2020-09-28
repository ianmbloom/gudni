   sweep :: forall axis
         . (Axis axis, axis~NextAxis(NextAxis axis))
         => axis
         -> Int
         -> With axis Bool
         -> With (NextAxis axis) Bool
         -> With axis s
         -> With (NextAxis axis) s
         -> Box s
         -> Branch axis s
         -> [(CurveTag, TaggedBezier s)]
         -> m (Branch axis s, [(CurveTag, TaggedBezier s)])
   sweep axis
         depth
         parallelIsMore
         perpendIsMore
         parentCut
         parentLine
         boundary
         mTree
         parentOverhangs =
       case mTree of
           Nothing -> do liftIO . putStrLn $ concat (replicate depth "   ") ++ "sweep Nothing"
                         return (Nothing, parentOverhangs)
           Just tree ->
               let bez = tree ^. confineCurve
                   itemTagId = tree ^. confineItemTagId
                   cut = tree ^. confineCut
                   lessBox = set (maxBox . athwart axis) (fromAxis axis cut) boundary
                   moreBox = set (minBox . athwart axis) (fromAxis axis cut) boundary
                   sweepLess = sweep (nextAxis axis) (depth + 1) perpendIsMore (onAxis axis False) parentLine cut lessBox (tree ^. confineLessCut)
                   sweepMore = sweep (nextAxis axis) (depth + 1) perpendIsMore (onAxis axis True ) parentLine cut moreBox (tree ^. confineMoreCut)
                   mess x = liftIO $ putStrLn $ concat (replicate depth "   ") ++ "sweep tag " ++ show (tree ^. confineCurveTag) ++ x
               in
               do  mess $ " pCut " ++ show parentCut ++ " pLine " ++ show parentLine ++ " boundary " ++ show boundary
                   mess $ " parent " ++ showOverhangs parentOverhangs
                   let continueOverhangs = filter (bezOverhangs axis parallelIsMore perpendIsMore boundary) $ moreOverhangs
                   (mLess, lessOverhangs) <- sweepLess parentOverhangs
                   let potentialOverhangs = (tree ^. confineCurveTag, TaggedBezier bez itemTagId):lessOverhangs
                   mess $ " potential " ++ showOverhangs potentialOverhangs
                   (mMore, moreOverhangs) <- sweepMore potentialOverhangs
                   mess $ " moreOverhangs " ++ showOverhangs moreOverhangs
                   modifiedNode <- foldM (addCrossing op axis parentCut parentLine) tree potentialOverhangs
                   mess $ " crossingsLess " ++ show (modifiedNode ^. confineCrossings)
                   --mess $ " continue "      ++ showOverhangs continueOverhangs
                   return ( Just .
                            set confineMoreCut mMore .
                            set confineLessCut mLess $
                            modifiedNode
                          , moreOverhangs
                          )
