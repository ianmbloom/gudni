{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Sweep
  ( sweepConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

sweepConfineTree :: forall s m
                 . ( Space s
                   , MonadIO m
                   )
                 => m ()
                 -> ConfineTree s
                 -> m (ConfineTree s)
sweepConfineTree op mTree =
   do (mTree', _) <- sweep Vertical
                           0
                           (onAxis Vertical   True    )
                           (onAxis Horizontal True    )
                           (onAxis Vertical   minBound)
                           (onAxis Horizontal minBound)
                           ( makeBox minBound
                                     minBound
                                     maxBound
                                     maxBound )
                           mTree
                           []
      return mTree'
   where
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
           Nothing -> do --liftIO . putStrLn $ concat (replicate depth "   ") ++ "sweep Nothing"
                         return (Nothing, parentOverhangs)
           Just tree ->
               let bez = tree ^. confineCurve
                   itemTagId = tree ^. confineItemTagId
                   cut = tree ^. confineCut
                   lessBox = set (maxBox . athwart axis) (fromAxis axis cut) boundary
                   moreBox = set (minBox . athwart axis) (fromAxis axis cut) boundary
                   sweepLess = sweep (nextAxis axis) (depth + 1) perpendIsMore (onAxis axis False) parentLine cut lessBox (tree ^. confineLessCut)
                   sweepMore = sweep (nextAxis axis) (depth + 1) perpendIsMore (onAxis axis True ) parentLine cut moreBox (tree ^. confineMoreCut)
                   --mess x = liftIO $ putStrLn $ concat (replicate depth "   ") ++ "sweep tag " ++ show (tree ^. confineCurveTag) ++ x
               in
               do  (mLess, lessOverhangs) <- sweepLess parentOverhangs
                   -- mess $ " pCut " ++ show parentCut ++ " pLine " ++ show parentLine ++ " boundary " ++ show boundary
                   -- mess $ " parent " ++ showOverhangs parentOverhangs
                   let potentialOverhangs = (tree ^. confineCurveTag, TaggedBezier bez itemTagId):lessOverhangs
                   -- mess $ " potential " ++ showOverhangs potentialOverhangs
                   (mMore, moreOverhangs) <- sweepMore potentialOverhangs
                   -- mess $ " moreOverhangs " ++ showOverhangs moreOverhangs
                   modifiedNode <- foldM (addCrossing op axis parentCut parentLine) tree potentialOverhangs
                   -- mess $ " crossingsLess " ++ show (modifiedNode ^. confineCrossings)
                   let continueOverhangs = filter (bezOverhangs axis parallelIsMore perpendIsMore boundary) $ moreOverhangs
                   -- mess $ " continue "      ++ showOverhangs continueOverhangs
                   return ( Just .
                            set confineMoreCut mMore .
                            set confineLessCut mLess $
                            modifiedNode
                          , continueOverhangs
                          )

   bezOverhangs :: Axis axis
                => axis
                -> With axis Bool
                -> With (NextAxis axis) Bool
                -> Box s
                -> (CurveTag, TaggedBezier s)
                -> Bool
   bezOverhangs axis
                parallelIsMore
                perpendIsMore
                boundary
                (curveTag, TaggedBezier bez _) =
       let box = boxOf bez
           maxAthwart = box ^. maxBox . athwart axis >= boundary ^. maxBox . athwart axis
           maxAlong   = box ^. maxBox . along   axis >= boundary ^. maxBox . along   axis
       in  maxAthwart || maxAlong

   showOverhangs :: [(CurveTag, TaggedBezier s)] -> String
   showOverhangs = show . map (\(curveTag, TaggedBezier _ itemTagId) -> (curveTag, itemTagId))

addCrossing :: forall axis s m
            .  ( Axis axis
               --, SwitchAxis (NextAxis axis)
               , axis~NextAxis(NextAxis (axis))
               , Monad m
               , Space s
               )
            => m ()
            -> axis
            -> With axis s
            -> With (NextAxis axis) s
            ->    Confine axis s
            -> (CurveTag, TaggedBezier s)
            -> m (Confine axis s)
addCrossing op axis parentCut parentLine tree (tag, TaggedBezier bez itemTagId) =
    --tcP ("-------- addCrossing from new tag: " ++ show tag ++ " over: " ++ show (tree ^. confineCurveTag)) $
    let start = parentCut
        end   = tree ^. confineCut
        tree' = if --trWhen (tag == checkTag) ("+newTag: " ++ show tag ++ " treeCurveTag: " ++ show (tree ^. confineCurveTag) ++ "   pLine " ++ show parentLine ++ " start: " ++ show start ++ " end: " ++ show end) $
                   crossesAlong (nextAxis axis) (unAxis parentLine) (unAxis start) (unAxis end) bez
                then over confineCrossings (toggleCrossing itemTagId) $
                     over confineCrossedCurves (tag:) $
                     tree
                else tree
    in  do op
           return tree'
