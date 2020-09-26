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
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw

import Graphics.Gudni.Util.Subdividable
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine
import qualified Data.Vector as V
import Control.Applicative
import Data.Maybe

data FacetState = FacetState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   ,_stateInsideAngle :: Angle SubSpace
   }
   deriving (Show)
makeLenses ''FacetState

instance HasStyle FacetState where
  type StyleOf FacetState = DefaultStyle

slantedLine :: Shape SubSpace
slantedLine = segmentsToShape [[Seg (Point2 0 0) Nothing, Seg (Point2 0.25 0) Nothing, Seg (Point2 1.25 1) Nothing, Seg (Point2 1 1) Nothing]]

instance Model FacetState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    --shouldLoop _ = False
    constructScene state _status =
       let angle   = state ^. stateBase . stateAngle
           repMode = state ^. stateBase . stateRepMode
           repDk   = state ^. stateBase . stateRepDk
           offset  = state ^. stateOffset
           point   = state ^. stateBase . stateCursor
           triangle :: V3 (Point2 SubSpace)
           triangle = V3 (Point2 0 0) (Point2 800 0) (Point2 0 800)

           pairTriangle = V3 (V2 (Point2 0 0)   (Point2 400 0  ))
                             (V2 (Point2 800 0) (Point2 800 800))
                             (V2 (Point2 0 800) (Point2 0 400))
           facets :: [Facet_ SubSpace]
           --facets = pure $ triangleToFacet triangle triangle
           facets = pure $ pairsToFacet pairTriangle triangle
           untilThreshold :: [Facet_ SubSpace]
           untilThreshold = fmap (traverseFacetUntil 0.5 point) $ facets
           traversed :: [Facet_ SubSpace]
           traversed = fmap (traverseFacet point) $ facets
           tesselatedFacets :: [Facet_ SubSpace]
           tesselatedFacets = {-trP "tesselated" $-} join . fmap (subdivideFacetSteps 1) $ facets
           fullyTesselated :: [Facet_ SubSpace]
           fullyTesselated  = join . fmap (tesselateFacet 1) $ facets

        in sceneFromLayout gray .
               transformFromState (state ^. stateBase) .
               overlap $
                   [ hatch 0.1 20 point
                   , stack $
                       [ --overlap . fmap (place . represent repDk) $ tesselatedFacets
                         overlap [ overlap . fmap (place . represent repDk) $ untilThreshold
                                 , overlap . fmap (place . represent repDk) $ traversed
                                 , overlap . fmap (place . represent repDk) $ facets
                                 ]
                       --, overlap . fmap (place . represent repDk) $ fullyTesselated

                       ]
                   ]
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
           , _stateCursor      = Point2 0 0
           }
       ) 0.75 (0 @@ deg)
