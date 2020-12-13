{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Application
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for defining an application that uses Gudni as a backend.

module Graphics.Gudni.Application
  ( runApplication
  , runApplicationDagOpenCL
  , runApplicationDagHaskell
  , Model(..)
  , ScreenMode(..)
  , SimpleTime
  , overState
  )
where
-- This must be compiled with the -threaded option

import Foreign.C
import Foreign.C.String
import Foreign

import Linear

import Debug.Trace
import System.Environment

import Data.Char
import Data.Maybe(fromMaybe)

import System.Exit (exitSuccess)
import System.Directory
import System.FilePath
import System.Timeout
import System.Clock
import System.Mem

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Monad.Random
import Control.Exception (evaluate)

import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.InterfaceSDL
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.ScreenMode
import Graphics.Gudni.Interface.Time
import Graphics.Gudni.Interface.FontLibrary
import Graphics.Gudni.Interface.Query

import Graphics.Gudni.Raster.OpenCL.Rasterizer
import Graphics.Gudni.Raster.Haskell.Rasterizer
import Graphics.Gudni.Raster.OpenCL.Instance


import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Raster.Class

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Raster.Serial.Pile

import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as S
import Data.Foldable
import System.Info

-- | The model typeclass is the primary interface to the application functions in Gudni
class ( HasStyle s
      )
      => Model s where
    -- | Construct a Scene from the state of type `s`
    constructScene   :: s -> String -> FontMonad (StyleOf s) IO (Scene (Layout (StyleOf s)))
    -- | Update the state based on the elapsed time and a list of inputs
    updateModelState :: Int -> SimpleTime -> [Input (TokenOf (StyleOf s))] -> s -> s
    -- | Do tasks in the IO monad based and update the current state.
    ioTask           :: MonadIO m => s -> m s
    ioTask state = return state
    -- | Set the initial display to FullScreen or a specific window size in pixels.
    screenSize       :: s -> ScreenMode
    -- | Determine if the application will enter the event loop.
    -- for debugging purposes you can set this to False and render one frame and quit.
    shouldLoop       :: s -> Bool
    shouldLoop _ = True
    -- | Path to the Truetype font file that is initially loaded.
    fontFile         :: s -> IO String
    fontFile _ = findDefaultFont
    -- | Bitmap texture data provided from the state for the rendered scene.
    providePictureMap :: s -> IO PictureMap
    providePictureMap _ = noPictures
    getCursor :: s -> Point2 PixelSpace
    getCursor _ = zeroPoint
    -- | Do something with the output of the rasterizer.
    handleOutput :: s -> DrawTarget -> StateT InterfaceState IO s
    handleOutput state target = do
        presentTarget target
        return state
    dumpState :: s -> [Input (TokenOf (StyleOf s))] -> IO ()
    dumpState _ _ = return ()

data ApplicationState r s = AppState
    { -- | The state maintained specific to the interface type.
      _appBackend       :: InterfaceState
      -- | Structure for marking time.
    , _appTimeKeeper    :: TimeKeeper
      -- | The start time of the application.
    , _appStartTime     :: TimeSpec
      -- | Constructor used to store the OpenCL state, compiled kernels and device metadata.
    , _appRasterizer    :: r
      -- | A string representing information about the app. Usually timing data and other stuff for display.
    , _appStatus        :: String
     -- | The number of event loop cycles that have commenced from starting.
    , _appCycle         :: Int
     -- | Polymorphic type defined by the client program. Represents the state of the client application.
    , _appState         :: s
    }
makeLenses ''ApplicationState

-- | Monad Stack for the event loop.
type ApplicationMonad r s = StateT (ApplicationState r s) (FontMonad (StyleOf s) IO)

runApplicationMonad :: ApplicationState r s -> ApplicationMonad r s a -> FontMonad (StyleOf s) IO a
runApplicationMonad = flip evalStateT

-- | Initializes openCL, frontend interface, timekeeper, randomfield data and returns the initial `ApplicationState`
setupApplication :: forall r s . (Rasterizer r, Model s) => r -> s -> IO (ApplicationState r s)
setupApplication rasterizer state  =
  do  -- Initialize the backend state.
      backendState <- startInterface (screenSize state)
      -- Start the timeKeeper
      timeKeeper <- startTimeKeeper
      startTime <- getTime Realtime
      return $ AppState backendState timeKeeper startTime rasterizer "No Status" 0 state

-- | Closes the interface.
closeApplication :: ApplicationMonad r s ()
closeApplication = withIO appBackend closeInterface

-- | Initialize the ApplicationMonad stack and enter the event loop.
startApplication :: ( Show s
                    , Model s
                    , Rasterizer r
                    , HasStyle s
                    , Show (TokenOf (StyleOf s))
                    , SpaceOf (StyleOf s) ~ SubSpace
                    )
                 => r
                 -> s
                 -> IO ()
startApplication rasterizer state =
    do  -- Initialize the application and get the initial state.
        appState <- setupApplication rasterizer state
        -- Start the glyph monad.
        runFontMonad $
            do  -- Load a font file.
                mFontFile <- liftIO $ fontFile state
                -- Add the font file to the glyph monad.
                addFont mFontFile
                -- Run the application monad.
                runApplicationMonad appState $
                    do  -- start the event loop.
                        loop []
                        -- when the loop exits close the application.
                        closeApplication

runApplicationDagOpenCL :: ( Show s
                           , Model s
                           , HasStyle s
                           , Show (TokenOf (StyleOf s))
                           , SpaceOf (StyleOf s) ~ SubSpace
                           )
                        => s
                        -> IO ()
runApplicationDagOpenCL state =
    do (rasterizer :: DagOpenCLState) <- setupRasterizer
       startApplication rasterizer state

runApplicationDagHaskell :: ( Show s
                            , Model s
                            , HasStyle s
                            , Show (TokenOf (StyleOf s))
                            , SpaceOf (StyleOf s) ~ SubSpace
                            )
                         => s
                         -> IO ()
runApplicationDagHaskell state =
    do (rasterizer :: DagHaskellState) <- setupRasterizer
       startApplication rasterizer state

runApplication :: ( Show s
                  , Model s
                  , HasStyle s
                  , Show (TokenOf (StyleOf s))
                  , SpaceOf (StyleOf s) ~ SubSpace
                  )
               => s
               -> IO ()
runApplication = runApplicationDagOpenCL
--runApplication = runApplicationDagHaskell

-- | Convert a `Timespec` to the `SimpleTime` (a double in seconds from application start)
toSimpleTime :: TimeSpec -> SimpleTime
toSimpleTime timeSpec = (fromIntegral . toNanoSecs $ timeSpec) / 1000000000

-- | Get the time elapsed from starting the event loop.
getElapsedTime :: ApplicationMonad r s SimpleTime
getElapsedTime =
  do  startTime   <- use appStartTime
      currentTime <- liftIO $ getTime Realtime
      return $ toSimpleTime $ currentTime `diffTimeSpec` startTime

-- | Debug message from the ApplicationMonad
appMessage :: String -> ApplicationMonad r s ()
appMessage = liftIO . putStrLn

-- | Call a function f in IO that uses the application state as it's first argument.
overState :: (MonadState g m) => Lens' g a -> (a -> m a) -> m ()
overState lens f =
  do  state <- use lens
      state' <- f state
      lens .= state'

-- | Initialize the timekeeper
restartAppTimer :: ApplicationMonad r s ()
restartAppTimer = appTimeKeeper <~ liftIO startTimeKeeper

-- | First phase of event loop.
beginCycle :: ApplicationMonad r s ()
beginCycle =
    do  restartAppTimer

-- | Update the model state and generate a shape tree, marking time along the way.
processState :: ( Show s
                , Model s
                , HasStyle s
                , Show (TokenOf (StyleOf s))
                )
             => SimpleTime
             -> [Input (TokenOf (StyleOf s))]
             -> ApplicationMonad r s (Scene (Layout (StyleOf s)), Point2 PixelSpace)
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        markAppTime "Advance State"
        state <- use appState
        let newState = updateModelState frame elapsedTime inputs state
        finalState <- liftIO (ioTask newState)
        appState .= finalState
        liftIO $ dumpState finalState inputs
        status <- use appStatus
        scene <- lift $ constructScene finalState status
        let cursor = getCursor finalState
        markAppTime "Build State"
        return (scene, cursor)

-- | Prepare and render the shapetree to a bitmap via the OpenCL kernel.
drawFrame :: ( Rasterizer r
             , Model s
             , Show (TokenOf (StyleOf s))
             , HasStyle s
             , SpaceOf (StyleOf s) ~ SubSpace
             )
          => Int
          -> Scene (Layout (StyleOf s))
          -> Point2 PixelSpace
          -> [PointQuery (SpaceOf (StyleOf s))]
          -> ApplicationMonad r s (DrawTarget, [PointQueryResult (TokenOf (StyleOf s))])
drawFrame frameCount scene cursor queries =
    do  --appMessage "ResetJob"
        rasterizer <- use appRasterizer
        target <- withIO appBackend (prepareTarget rasterizer)
        let canvasSize = P $ fromIntegral <$> target ^. targetArea
        state <- use appState
        pictureMap <- liftIO $ providePictureMap state
        markAppTime "Build TileTree"
        lift $ rasterFrame rasterizer canvasSize pictureMap scene frameCount queries cursor target
        return (target, []) --  toList queryResults)

-- Final phase of the event loop.
endCycle :: SimpleTime -> ApplicationMonad r s ()
endCycle elapsedTime =
    do  tk         <- use appTimeKeeper
        cycleCount <- use appCycle
        let status = showTimes "Loop Cycle" True tk
                   -- ++ show job
                   ++ "------ Cycle: "
                   ++ show cycleCount
                   ++ "\n------ Elapsed Time: "
                   ++ showFlFixed' 2 1 elapsedTime ++ "\n"
        appStatus .= status
        when (os == "darwin") $ appMessage status
        appCycle += 1

-- Mark a time in the status.
markAppTime :: String -> ApplicationMonad r s ()
markAppTime message =
    do  tk  <- use appTimeKeeper
        tk' <- liftIO $ markTime message tk
        appTimeKeeper .= tk'

-- | Determine if a particular input should quit the application.
isQuit :: Input token -> Bool
isQuit input =
    case (input ^. inputType) of
        InputWindow WindowClosed -> True
        InputKey Pressed (KeyModifier _ _ _ True) (Key LetterQ) -> True
        InputKey Pressed _ (KeyCommand CommandQuit) -> True
        _ -> False

-- | Cycle through the event loop.
loop :: ( Rasterizer r
        , Show s
        , Model s
        , HasStyle s
        , Show (TokenOf (StyleOf s))
        , (SpaceOf (StyleOf s)) ~ SubSpace
        )
     => [Input (TokenOf (StyleOf s))]
     -> ApplicationMonad r s ()
loop preppedInputs =
  do  --appMessage "checkInputs"
      unless (any isQuit preppedInputs) $
          do  elapsedTime <- getElapsedTime
              beginCycle
              (scene, cursor) <- processState elapsedTime preppedInputs
              frameCount <- fromIntegral <$> use appCycle
              newInputs <- withIO appBackend checkInputs
              let queries = pullQueries newInputs
              (target, queryResults) <- drawFrame frameCount scene cursor (map (over pointQueryPos (fmap realToFrac)) queries)
              let newPreppedInputs = tr "preppedInputs" $ attachQueryResults newInputs queryResults
              state  <- use appState
              state' <- withIO appBackend $ handleOutput state target
              appState .= state'
              markAppTime "Raster Frame"
              endCycle elapsedTime
              liftIO performMinorGC -- the idea here is that if we perform garbage collection
                                    -- on each frame we'll get a more consistent frame rate.
              continue <- shouldLoop <$> use appState
              when continue (loop newPreppedInputs)
