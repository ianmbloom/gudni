{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ScaffoldTree
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug

import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
import Control.Monad
import Control.Monad.State

import System.Info

data Tree = Branch Tree Tree | Leaf Int deriving (Show)

data TreeState = TreeState
  { _stateBase        :: BasicSceneState
  , _stateTree        :: Tree
  } deriving (Show)
makeLenses ''TreeState

initialModel tree =
    TreeState
    { _stateBase = BasicSceneState
        { _stateScale       = 30
        , _stateDelta       = Point2 0 0
        , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 10
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 0
        }
    , _stateTree        = tree
    }

instance Model TreeState where
    screenSize state = --FullScreen
                       Window $ Point2 1024 900
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        let state' = foldl (flip processInput) state inputs
        in  over stateBase (updateSceneState frame elapsedTime) state'
    ioTask = return
    constructScene state status =
        do  let treeScaffold = undefined -- scafTree (state ^. stateTree)
            testScene <- scaffoldToSTree treeScaffold
            statusTree <- (^?! unGlyph) <$> statusDisplay (state ^. stateBase) "Test Paragraph" (lines status)
            let tree = transformFromState (state ^. stateBase) testScene
                withStatus = if False then overlap [statusTree, tree] else tree
            return $ (Scene (light gray) $ withStatus)
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

instance HandlesInput TreeState where
    processInput input = over stateBase (processInput input)

main :: IO ()
main = --silence $
       do putStrLn "Started"
          runApplication (initialModel (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) :: TreeState)

-- | Translate tree data structure to scaffold.
--scafTree :: Monad m => Tree -> Scaffold m Compound (Glyph (CompoundTree SubSpace))
scafTree (Branch l r) =
  let left  = SLeaf $ Named "left"  $ scafTree l
      right = SLeaf $ Named "right" $ scafTree r
  in  SLeaf $ Named "origin" $
      overlap [ stack AlignMin [left, right]
              , SLeaf $ Build2 (line 0.1) (From ["left" , "origin"]) (Offset zeroPoint)
              , SLeaf $ Build2 (line 0.1) (From ["right", "origin"]) (Offset zeroPoint)
              ]
scafTree (Leaf i) = SLeaf $ Named "origin" $ SLeaf $ Build circle
