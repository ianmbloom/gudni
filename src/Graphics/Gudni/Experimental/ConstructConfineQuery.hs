{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Experimental.ConstructConfineQuery
  ( constructLayerStack
  , checkPoint
  , checkBox
  --, tracePoint
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Layout
import Graphics.Gudni.Experimental.ConfineTree
--import Graphics.Gudni.Experimental.ConstructConfineTree
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.PrettyPrint.GenericPretty

cross :: IsStyle style
      => Point2 (SpaceOf style)
      -> SpaceOf style
      -> SpaceOf style
      -> Layout style
cross point thickness size =
  let s = size / 2
  in
  translateBy point .
  withColor black $
  overlap [mask . stroke thickness . makeOpenCurve $ [line (Point2 (-s) (-s)) (Point2 s s)]
          ,mask . stroke thickness . makeOpenCurve $ [line (Point2 s (-s)) (Point2 (-s) s)]
          ]

ring :: IsStyle style
     => Int
     -> Color
     -> Layout style
ring layer color =
    let r = 4
        l = fromIntegral layer
    in
    if layer > 0
    then withColor color $ (scaleBy (l * r) . mask $ circle) `subtractFrom` (scaleBy ((l + 1) * r) . mask $ circle)
    else withColor color $ (scaleBy ((l + 1) * r) . mask $ circle)

layerRing :: forall token style
           .  (IsStyle style)
           => M.Map ItemTagId Color
           -> Point2 (SpaceOf style)
           -> Int
           -> ItemTagId
           -> Layout style
layerRing colorMap point depth itemTagId =
  let color = transparent 1 $ (M.!) colorMap itemTagId
  in  translateBy point $ ring (depth + 1) color

constructLayerStack :: forall style
                    .  ( IsStyle style
                       )
                    => M.Map ItemTagId Color
                    -> Point2 (SpaceOf style)
                    -> [ItemTagId]
                    -> Layout style
constructLayerStack colorMap point stack =
  do  --let string = ((concat $ imap (\i a -> "(iT " ++ show (a ^. ancItemTagId) ++ " tg " ++ show (a ^. ancTag) ++ " c " ++ show (a ^. ancCornerWinding) ++ " w " ++ show (a ^. ancWinding) ++ ")" ) stack)) ++ "\n" ++
      --             intercalate "\n" (map show steps)
      --let string = show . sum . map (length . view layTags) $ stack -- intercalate "\n" $ map (show . view layTags) stack
      --text <- fromGlyph <$> paragraph 0.1 0.1 AlignMin AlignMin string :: FontMonad m (CompoundTree SubSpace)
      overlap $ cross point 1 8:(imap (layerRing colorMap point) $ stack) -- ++ [withColor black . translateBy point . translateByXY 4 (-7.5) . scaleBy 15 $ text]

checkPoint :: forall style
           .  ( IsStyle style
              )
           => M.Map ItemTagId Color
           -> ConfineTree (SpaceOf style)
           -> Point2 (SpaceOf style)
           -> Layout style
checkPoint colorMap tree point =
  let stack = pointWinding tree point
  in  constructLayerStack colorMap point stack

checkBox :: forall style
         .  (IsStyle style)
         => M.Map ItemTagId Color
         -> ConfineTree (SpaceOf style)
         -> Point2 (SpaceOf style)
         -> Layout style
checkBox colorMap tree point =
  let box = Box point (point + Point2 50 50)
      boxStack = breakPixel tree box
  in  overlap <$> map (withColor black . mask . openRectangle 1) $ map fst boxStack

{-
tracePoint :: forall m token
           .  (Monad m, Show token)
           => M.Map ItemTagId Color
           -> ConfineTree SubSpace
           -> Point2 SubSpace
           -> Int
           -> FontMonad m (ShapeTree token SubSpace)
tracePoint colorMap tree point i =
  let (stack, steps) = pointWinding tree point
      step = steps ^?! ix (clamp 0 (length steps - 1) i)
  in
  do  confine <- case step ^. stepConfine of
                   Left  vTree -> constructConfine Vertical   vTree reasonableBoundaries
                   Right hTree -> constructConfine Horizontal hTree reasonableBoundaries
      let corner = case step ^. stepConfine of
                      Left  vTree -> confineAnchorPoint vTree
                      Right hTree -> confineAnchorPoint hTree
      let string = "horiWind " ++ show (step ^. stepHoriWind) ++ "\nvertWind " ++ show (step ^. stepVertWind)
      text <- fromGlyph . withColor (dark green) . translateBy corner . translateByXY 4 0 . scaleBy 30 <$> paragraph 0.1 0.1 AlignMin AlignMin string
      label <- constructAnchorStack colorMap point (step ^. stepStack) []
      return . overlap $ [label, text, confine]
-}
