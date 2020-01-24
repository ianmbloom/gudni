{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Paragraph
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad.State

data ParagraphState = ParagraphState
  { _stateBase        :: BasicSceneState
  } deriving (Show)
makeLenses ''ParagraphState

initialModel =
    ParagraphState
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
    }

instance Model ParagraphState where
    screenSize state = --FullScreen
                       Window $ Point2 1024 900
    updateModelState frame elapsedTime inputs state =
        let state' = foldl (flip processInput) state inputs
        in  over stateBase (updateSceneState frame elapsedTime) state'
    constructScene state status =
        do  para <- paragraph 0.2 0.2 AlignCenter AlignCenter mobyDick

            let testScene :: ShapeTree Int SubSpace
                testScene = (^?! unGlyph) . colorWith black . rack AlignMin $ distributeRack 0 $ [para, scaleBy 10 circle]
            --statusTree <- (^?! unGlyph) <$> statusDisplay (state ^. stateBase) "Test Paragraph" (lines status)
            let tree = transformFromState (state ^. stateBase) testScene
                withStatus = {-if False then overlap [statusTree, tree] else-} tree
            return $ (Scene (light gray) $ withStatus)

instance HandlesInput token ParagraphState where
    processInput input = over stateBase (processInput input)

main :: IO ()
main = --silence $
       do putStrLn "Started"
          runApplication (initialModel :: ParagraphState)

-- | Sample text paragraph.
mobyDick :: String
mobyDick
  =  "Call me Ishmael. Some years ago--never mind how long precisely--having little\n"
  ++ "or no money in my purse, and nothing particular to interest me on shore, I thought\n"
  ++ "I would sail about a little and see the watery part of the world. It is a way I have of\n"
  ++ "driving off the spleen and regulating the circulation. Whenever I find myself growing\n"
  ++ "grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I\n"
  ++ "find myself involuntarily pausing before coffin warehouses, and bringing up the rear of\n"
  ++ "every funeral I meet; and especially whenever my hypos get such an upper hand of me,\n"
  ++ "that it requires a strong moral principle to prevent me from deliberately stepping into\n"
  ++ "the street, and methodically knocking people's hats off--then, I account it high time\n"
  ++ "to get to sea as soon as I can. This is my substitute for pistol and ball. With a\n"
  ++ "philosophical flourish Cato throws himself upon his sword; I quietly take to the ship.\n"
  ++ "There is nothing surprising in this. If they but knew it, almost all men in their degree,\n"
  ++ "some time or other, cherish very nearly the same feelings towards the ocean with me.\n"
  ++ "There now is your insular city of the Manhattoes, belted round by wharves as Indian isles\n"
  ++ "by coral reefs--commerce surrounds it with her surf. Right and left, the streets take you\n"
  ++ "waterward. Its extreme downtown is the battery, where that noble mole is washed by waves,\n"
  ++ "and cooled by breezes, which a few hours previous were out of sight of land. Look at the\n"
  ++ "crowds of water-gazers there. Circumambulate the city of a dreamy Sabbath afternoon. Go\n"
  ++ "from Corlears Hook to Coenties Slip, and from thence, by Whitehall, northward. What do\n"
  ++ "you see?--Posted like silent sentinels all around the town, stand thousands upon thousands\n"
  ++ "of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the\n"
  ++ "pier-heads; some looking over the bulwarks of ships from China; some high aloft in the rigging,\n"
  ++ "as if striving to get a still better seaward peep. But these are all landsmen; of week days pent\n"
  ++ "up in lath and plaster--tied to counters, nailed to benches, clinched to desks. How then is this?\n"
  ++ "Are the green fields gone? What do they here? But look! here come more crowds, pacing straight for twater,\n"
  ++ "and seemingly bound for a dive. Strange! Nothing will content them but the extremest limit of the land; \n"
  ++ "loitering under the shady lee of yonder warehouses will not suffice. No. They must get just as nigh the\n"
  ++ "water as they possibly can without falling in. And there they stand--miles of them--leagues. Inlanders all,\n"
  ++ "they come from lanes and alleys, streets and avenues--north, east, south, and west. Yet here they all unite.\n"
  ++ "Tell me, does the magnetic virtue of the needles of tcompasses of all those ships attract them thither?\n"
