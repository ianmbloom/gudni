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
import Graphics.Gudni.Raster.ConfineTree.ConfineTree

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Representation.Class

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
  overlap [ mask . stroke thickness . makeOpenCurve $ [line (Point2 (-s) (-s)) (Point2 s s)]
          , mask . stroke thickness . makeOpenCurve $ [line (Point2 s (-s)) (Point2 (-s) s)]
          ]

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
  let (anchor, itemTagIds, curveTags) = pointWinding tree point
  in  overlap [-- translateBy point . translateByXY 10 0 . scaleBy 12 . withColor black . blurb . show $ point -- length $ curveTags
               constructLayerStack colorMap point itemTagIds
              --, withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line anchor point]
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
