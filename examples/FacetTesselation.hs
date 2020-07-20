{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProjectDemo
-- Copyright   :  (c) Daniel Bergey 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Demonstration of equal-distance projectOnto, and equal spacing in t-paramater.

module FacetTesselation
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Subdividable
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug

import qualified Graphics.Gudni.Figure.Bezier as B

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine
import qualified Data.Vector as V
import Control.Applicative


import Data.Maybe
import Control.Lens

data FacetState = FacetState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   ,_stateInsideAngle :: Angle SubSpace
   }
   deriving (Show)
makeLenses ''FacetState

instance HasToken FacetState where
  type TokenOf FacetState = Int

slantedLine :: Shape SubSpace
slantedLine = segmentsToShape [[Seg (Point2 0 0) Nothing, Seg (Point2 0.25 0) Nothing, Seg (Point2 1.25 1) Nothing, Seg (Point2 1 1) Nothing]]
instance Model FacetState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        do text <- fromGlyph <$> blurb 0.1 AlignMin "e" -- "Georg Guðni Hauksson"
           let angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
           return . Scene gray $
               --(if repMode then represent repDk else id) $
               ((transformFromState {-(set stateAngle (0 @@ deg)-} (state ^. stateBase){-)-} $
               overlap [ translateByXY 0   0 $ overlap [represent repDk projectedFacet
                                                       , withColor (dark gray) . mask . shapeFrom . stroke 3 $ path
                                                       ]
                       , translateByXY 100 0 $ overlap [ overlap . fmap (represent repDk) $ tesselatedFacets
                                                       , withColor (dark gray) . mask . shapeFrom . stroke 3 $ path
                                                       ]
                       , translateByXY 100 0 $ overlap [ overlap . fmap (represent repDk) $ tesselatedFacets
                                                       , withColor (dark gray) . mask . shapeFrom . stroke 3 $ path
                                                       ]
                       ]) :: ShapeTree Int SubSpace)
      where
        bzX = Bez (Point2 0 0) (Point2 0.5 1) (Point2 1 0) :: Bezier SubSpace
        bz1 = Bez (Point2 20 0) (Point2 0   0) (Point2 0 40)
        bz2 = Bez (Point2 0 40) (Point2 0 80) (Point2 40 80)
        bz3 = Bez (Point2 40 80) (Point2 80 80) (Point2 80 160)
        bz4 = Bez (Point2 80 160) (Point2 80 300) (Point2 160 300)
        path :: OpenCurve_ V.Vector SubSpace
        path = makeOpenCurve [bz1, bz2, bz3, bz4]
        triangle :: V3 (Point2 SubSpace)
        triangle = V3 (Point2 0 0) (Point2 100 0) (Point2 0 100)
        facet :: Facet
        facet = triangleToFacet triangle triangle
        projectedFacet :: Facet
        projectedFacet = projectOnto False path facet
        tesselatedFacets :: [Facet]
        tesselatedFacets = tesselateFacetSteps 1 $ projectedFacet
        fullyTesselated :: [Facet]
        fullyTesselated  = tesselateFacet 0.5 $ projectedFacet

    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

instance HandlesInput token FacetState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case (input ^. inputType) of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 10.01
                          Key ArrowLeft  -> stateOffset -= 10.01
                          Key ArrowUp    -> stateInsideAngle %= normalizeAngle . (^+^ (3 @@ deg))
                          Key ArrowDown  -> stateInsideAngle %= normalizeAngle . (^-^ (3 @@ deg))
                          _ -> return ()

              _ -> return ()
          )

marker :: Color -> Point2 SubSpace -> ShapeTree Int SubSpace
marker color center = withColor (transparent 0.5 color) $ translateBy center marker0

marker0 :: CompoundTree SubSpace
marker0 = {-rotateBy (1/8 @@ turn) $ translateBy (Point2 (s/2) (s/2)) $-} square
    where
        s = 8
        square :: CompoundTree SubSpace
        square = rectangle (Point2 s s)

main :: IO ()
main = runApplication $ FacetState
       (BasicSceneState
           { _stateScale       = 1
           , _stateDelta       = Point2 0 0
           , _stateAngle       = 0 @@ deg
           , _statePaused      = True
           , _stateSpeed       = 1
           , _statePace        = 10
           , _stateLastTime    = 0
           , _stateDirection   = True
           , _statePlayhead    = 0
           , _stateFrameNumber = 0
           , _stateStep        = 69
           , _stateRepMode     = False
           , _stateRepDk       = False
           }
       ) 0.75 (0 @@ deg)
