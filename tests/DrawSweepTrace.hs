{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module DrawSweepTrace
  ( constructSweepTrace
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.ConfineTree.SweepTrace
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Traverse
import Graphics.Gudni.Raster.Dag.State

import Graphics.Gudni.Draw
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Foreign.Storable
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M
import qualified Data.Colour.Names as CN

simpleBezier :: (IsStyle style)
             => Color (SpaceOf style)
             -> Bezier (SpaceOf style)
             -> Layout style
simpleBezier color bz =
     withColor color . mask . stroke 1 $ makeOpenCurve [bz]

boxedBezier :: (IsStyle style)
            => Color (SpaceOf style)
            -> Color (SpaceOf style)
            -> Bezier (SpaceOf style)
            -> Layout style
boxedBezier backColor color bz =
  let box = boxOf bz
  in  overlap [ withColor color     . mask . stroke 1 $ makeOpenCurve [bz]
              , withColor backColor . mask . boxToRectangle $ box
              ]

pathLine :: IsStyle style => Color (SpaceOf style) -> (Point2 (SpaceOf style), Point2 (SpaceOf style)) -> Layout style
pathLine  color (start, end) = withColor (transparent 0.5 color) . mask . stroke 4 . makeOpenCurve $ [line (constrainPoint start) (constrainPoint end)]

constructSweepStored :: forall style
                     .  IsStyle style
                     => Bool
                     -> Int
                     -> [Primitive (SpaceOf style)]
                     -> Layout style
constructSweepStored bypassed i prims =
  let sat = if bypassed then saturate 0.25 else id
      color :: Color (SpaceOf style)
      color = sat $ colorList !! (i `mod` numColors)
  in
  overlap $ map (withColor color . drawPrim) prims

constructSweepTrace :: forall style m
                    . ( IsStyle style
                      , Storable (SpaceOf style)
                      , MonadIO m
                      )
                    => SweepTrace (SpaceOf style)
                    -> RayMonad (SpaceOf style) m (Layout style)
constructSweepTrace trace =
   do discarded <- mapM loadPrimT (trace ^. sweepDiscarded)
      continue  <- mapM loadPrimT (trace ^. sweepContinue)
      bypass    <- mapM (mapM loadPrimT) (trace ^. sweepBypasses)
      return $
          overlap
              [ overlap $ map (pathLine blue  ) (nullTail $ trace ^. sweepPath)
              , overlap $ map (pathLine purple) (nullHead $ trace ^. sweepPath)
              , overlap $ map (withColor black  . drawPrim) discarded
              , overlap $ map (withColor yellow . drawPrim) continue
              , overlap $ imap (constructSweepStored True) $ bypass
              , overlap $ map (withColor (transparent 0.5 $ light gray) . mask . boxToRectangle . constrainBox) (trace ^. sweepVisited)
              ]

colorList :: Space s => [Color s]
colorList = [ red, orange, yellow, green, blue, purple ]

numColors :: Int
numColors = length (colorList :: [Color SubSpace])
