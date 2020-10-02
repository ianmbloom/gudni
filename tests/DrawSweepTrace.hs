{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}

module DrawSweepTrace
  ( constructSweepTrace
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.SweepTrace
--import Graphics.Gudni.Raster.ConfineTree.Sweep

import Graphics.Gudni.Draw
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M
import qualified Data.Colour.Names as CN

simpleBezier :: (IsStyle style)
             => Color
             -> TaggedBezier (SpaceOf style)
             -> Layout style
simpleBezier color (TaggedBezier bz _ _) =
     withColor color . mask . stroke 1 $ makeOpenCurve [bz]

boxedBezier :: (IsStyle style)
            => Color
            -> Color
            -> TaggedBezier (SpaceOf style)
            -> Layout style
boxedBezier backColor color (TaggedBezier bz _ _) =
  let box = boxOf bz
  in  overlap [ withColor color . mask . stroke 1 $ makeOpenCurve [bz]
              , withColor backColor . mask . boxToRectangle $ box
              ]

pathLine :: IsStyle style => Color -> (Point2 (SpaceOf style), Point2 (SpaceOf style)) -> Layout style
pathLine  color (start, end) = withColor (transparent 0.5 color) . mask . stroke 4 . makeOpenCurve $ [line (constrainPoint start) (constrainPoint end)]

constructSweepStored :: IsStyle style
                     => Bool
                     -> Int
                     -> [TaggedBezier (SpaceOf style)]
                     -> Layout style
constructSweepStored bypassed i curves =
  let sat = if bypassed then saturate 0.25 else id
      color = sat $ colorList !! (i `mod` numColors)
  in
  overlap $ map (simpleBezier color) curves
      --  withColor color . translateBy (box ^. topLeftBox) . scaleBy 20 . blurb $ show i
      --, overlap $
      --]

constructSweepTrace :: forall style
                    .  (IsStyle style
                       )
                    => SweepTrace (SpaceOf style)
                    -> Layout style
constructSweepTrace trace =
    overlap
        [ overlap $ map (pathLine blue) (nullTail $ trace ^. sweepPath)
        , overlap $ map (pathLine purple) (nullHead $ trace ^. sweepPath)
        , overlap $ map (simpleBezier black) (trace ^. sweepDiscarded)
        , overlap $ map (simpleBezier yellow) (trace ^. sweepContinue)
        , overlap $ imap (constructSweepStored True) $ (trace ^. sweepBypasses)
        , overlap $ map (withColor (transparent 0.5 $ light gray) . mask . boxToRectangle . constrainBox) (trace ^. sweepVisited)
        ]

numColors = length colorList

colorList =
    map colourToColor
    [ CN.aliceblue
    , CN.antiquewhite
    , CN.aqua
    , CN.aquamarine
    , CN.azure
    , CN.beige
    , CN.bisque
    , CN.blanchedalmond
    , CN.blue
    , CN.blueviolet
    , CN.brown
    , CN.burlywood
    , CN.cadetblue
    , CN.chartreuse
    , CN.chocolate
    , CN.coral
    , CN.cornflowerblue
    , CN.cornsilk
    , CN.crimson
    , CN.cyan
    , CN.darkviolet
    , CN.deeppink
    , CN.deepskyblue
    , CN.firebrick
    , CN.floralwhite
    , CN.forestgreen
    , CN.fuchsia
    , CN.gainsboro
    , CN.ghostwhite
    , CN.gold
    , CN.goldenrod
    , CN.green
    , CN.greenyellow
    , CN.honeydew
    , CN.hotpink
    , CN.indianred
    , CN.indigo
    , CN.ivory
    , CN.khaki
    , CN.lavender
    , CN.lavenderblush
    , CN.lawngreen
    , CN.lemonchiffon
    , CN.lime
    , CN.limegreen
    , CN.linen
    , CN.magenta
    , CN.maroon
    , CN.midnightblue
    , CN.mintcream
    , CN.mistyrose
    , CN.moccasin
    , CN.navajowhite
    , CN.navy
    , CN.oldlace
    , CN.olive
    , CN.olivedrab
    , CN.orange
    , CN.orangered
    , CN.orchid
    , CN.palegoldenrod
    , CN.palegreen
    , CN.paleturquoise
    , CN.palevioletred
    , CN.papayawhip
    , CN.peachpuff
    , CN.peru
    , CN.pink
    , CN.plum
    , CN.powderblue
    , CN.purple
    , CN.red
    , CN.rosybrown
    , CN.royalblue
    , CN.saddlebrown
    , CN.salmon
    , CN.sandybrown
    , CN.seagreen
    , CN.seashell
    , CN.sienna
    , CN.silver
    , CN.skyblue
    , CN.slateblue
    , CN.snow
    , CN.springgreen
    , CN.steelblue
    , CN.teal
    , CN.thistle
    , CN.tomato
    , CN.turquoise
    , CN.violet
    , CN.wheat
    , CN.white
    , CN.whitesmoke
    , CN.yellow
    , CN.yellowgreen
    ]
