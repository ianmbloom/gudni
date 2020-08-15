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

instance HasStyle FacetState where
  type StyleOf FacetState = DefaultStyle

slantedLine :: Shape SubSpace
slantedLine = segmentsToShape [[Seg (Point2 0 0) Nothing, Seg (Point2 0.25 0) Nothing, Seg (Point2 1.25 1) Nothing, Seg (Point2 1 1) Nothing]]

instance Model FacetState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        do let angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
           sceneFromLayout gray .
               transformFromState (state ^. stateBase) .
               stack $
               [ overlap [  overlap . fmap (place . represent repDk) $ unFacetGroup projectedFacet
                         , withColor (dark gray) . mask . stroke 3 $ path
                         ]
               , overlap [ overlap . fmap (place . represent repDk) $ unFacetGroup tesselatedFacets
                         , withColor (dark gray) . mask . stroke 3 $ path
                         ]
               , overlap [ overlap . fmap (place . represent repDk) $ unFacetGroup tesselatedFacets
                         , withColor (dark gray) . mask . stroke 3 $ path
                         ]
               ]
      where
        bzX = Bez (Point2 0 0) (Point2 0.5 1) (Point2 1 0) :: Bezier SubSpace
        bz1 = Bez (Point2 20 0) (Point2 0   0) (Point2 0 40)
        bz2 = Bez (Point2 0 40) (Point2 0 80) (Point2 40 80)
        bz3 = Bez (Point2 40 80) (Point2 80 80) (Point2 80 160)
        bz4 = Bez (Point2 80 160) (Point2 80 300) (Point2 160 300)
        path :: OpenCurve SubSpace
        path = makeOpenCurve (pure bz1 <|> pure bz2 <|> pure bz3 <|> pure bz4)
        triangle :: V3 (Point2 SubSpace)
        triangle = V3 (Point2 0 0) (Point2 100 0) (Point2 0 100)
        facets :: FacetGroup SubSpace
        facets = FacetGroup $ pure $ triangleToFacet triangle triangle
        projectedFacet :: FacetGroup SubSpace
        projectedFacet = projectDefault False (makeBezierSpace arcLength $ path) facets
        tesselatedFacets :: FacetGroup SubSpace
        tesselatedFacets = tesselateFacetSteps 1 $ projectedFacet
        fullyTesselated :: FacetGroup SubSpace
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
