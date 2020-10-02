{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Draw.Representation.ConfineQuery
  ( constructLayerStack
  , checkPoint
  , ring
  --, tracePoint
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Layout
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.Query

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Symbols
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.Representation.Class

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Subdividable

import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.PrettyPrint.GenericPretty

ring :: IsStyle style
     => SpaceOf style
     -> Int
     -> Color
     -> Layout style
ring r layer color =
    let l = fromIntegral layer
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
  in  translateBy point $ ring 4 (depth + 1) color

constructLayerStack :: forall style
                    .  ( IsStyle style
                       )
                    => M.Map ItemTagId Color
                    -> Point2 (SpaceOf style)
                    -> ItemStack
                    -> Layout style
constructLayerStack colorMap point stack =
  do  --let string = ((concat $ imap (\i a -> "(iT " ++ show (a ^. ancItemTagId) ++ " tg " ++ show (a ^. ancTag) ++ " c " ++ show (a ^. ancCornerWinding) ++ " w " ++ show (a ^. ancWinding) ++ ")" ) stack)) ++ "\n" ++
      --             intercalate "\n" (map show steps)
      --let string = show . sum . map (length . view layTags) $ stack -- intercalate "\n" $ map (show . view layTags) stack
      --text <- fromGlyph <$> paragraph 0.1 0.1 AlignMin AlignMin string :: FontMonad m (CompoundTree SubSpace)
      overlap $ hatch 1 8 point:(imap (layerRing colorMap point) $ stack) -- ++ [withColor black . translateBy point . translateByXY 4 (-7.5) . scaleBy 15 $ text]

checkPoint :: forall style
           .  ( IsStyle style
              )
           => M.Map ItemTagId Color
           -> ConfineTree (SpaceOf style)
           -> DecorateTree (SpaceOf style)
           -> Point2 (SpaceOf style)
           -> Layout style
checkPoint colorMap confineTree decoTree point =
  let (anchor, anchorStack, stack, curveTags) = queryPointWithInfo confineTree decoTree point
      makeList a = [a]
      bezList :: [Bezier (SpaceOf style)]
      bezList = concat $ map ({-subdivideBeziers 4 .-}  makeList . view tBez) $ curveTags
      numCrossings = length . filter id . map (crosses anchor point) $ bezList
      markBez bez = let ch  = crossesHorizontal anchor point bez
                        cv  = crossesVertical   anchor point bez
                        sl  = bezierSlopeLTEZero Vertical bez
                    in  (ch, cv, sl, bez)
      drawCurve :: (Bool, Bool, Bool, Bezier (SpaceOf style)) -> Layout style
      drawCurve (ch, cv, sl, bez) =
          let color = case (ch, cv, sl) of
                         (True,  True,  True ) -> red
                         (True,  True,  False) -> light (light red)
                         (True,  False, True ) -> purple
                         (True,  False, False) -> light (light purple)
                         (False, True,  True ) -> blue
                         (False, True,  False) -> light (light blue)
                         (False, False, True ) -> green
                         (False, False, False) -> yellow
          in  withColor color . mask . stroke 0.1 . makeOpenCurve . makeList $ bez
      curves = overlap $ map drawCurve $ {-tr "markBez" $-} map markBez bezList
      label :: Layout style
      label = if length stack > 0
              then translateBy point . translateByXY 10 0 . scaleBy 12 . withColor black . blurb . show $ length bezList
              else emptyItem
  in  overlap [
              -- label
              --,
              -- constructLayerStack colorMap anchor anchorTags
              --,
               constructLayerStack colorMap point stack
              --,
              -- curves
              --,
              -- withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line anchor  point]
              -- ,
              --  withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line anchor (interimPoint anchor point)]
              -- ,
              --  withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line (interimPoint anchor point) point]
              ]
{-
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
-}
