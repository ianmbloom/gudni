{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}

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
import Control.DeepSeq

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
import Graphics.Gudni.Interface.Token

import Graphics.Gudni.OpenCL.Setup
import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.ProcessBuffers

import Graphics.Gudni.Raster.Constants (rANDOMFIELDsIZE)
import Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.Params
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.RandomField

import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as S
import Data.Foldable
import System.Info

-- | The model typeclass is the primary interface to the application functions in Gudni
class HasToken s => Model s where
  -- | Construct a Scene from the state of type `s`
  constructScene  :: s -> String -> FontMonad IO (Scene (ShapeTree (TokenOf s) SubSpace))
  -- | Update the state based on the elapsed time and a list of inputs
  updateModelState :: Int -> SimpleTime -> [Input (TokenOf s)] -> s -> s
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
  -- | Do something with the output of the rasterizer.
  handleOutput :: s -> DrawTarget -> StateT InterfaceState IO s
  handleOutput state target = do
      presentTarget target
      return state

data ApplicationState s = AppState
    { -- | The state maintained specific to the interface type.
      _appBackend       :: InterfaceState
      -- | Structure for marking time.
    , _appTimeKeeper    :: TimeKeeper
      -- | The start time of the application.
    , _appStartTime     :: TimeSpec
      -- | Constructor used to store the OpenCL state, compiled kernels and device metadata.
    , _appRasterizer :: Rasterizer
      -- | A string representing information about the app. Usually timing data and other stuff for display.
    , _appStatus        :: String
     -- | The number of event loop cycles that have commenced from starting.
    , _appCycle         :: Int
     -- | Polymorphic type defined by the client program. Represents the state of the client application.
    , _appState         :: s
    }
makeLenses ''ApplicationState

-- | Monad Stack for the event loop.
type ApplicationMonad s = StateT (ApplicationState s) (FontMonad IO)

runApplicationMonad :: ApplicationState s -> ApplicationMonad s a -> FontMonad IO a
runApplicationMonad = flip evalStateT

-- | Initializes openCL, frontend interface, timekeeper, randomfield data and returns the initial `ApplicationState`
setupApplication :: Model s => s -> IO (ApplicationState s)
setupApplication state  =
  do  -- Setup OpenCL state and kernels.
      openCLLibrary <- setupOpenCL False False embeddedOpenCLSource
      -- Initialize the backend state.
      backendState <- startInterface (screenSize state)
      -- Start the timeKeeper
      timeKeeper <- startTimeKeeper
      startTime <- getTime Realtime
      return $ AppState backendState timeKeeper startTime openCLLibrary "No Status" 0 state

-- | Closes the interface.
closeApplication :: ApplicationMonad s ()
closeApplication = withIO appBackend closeInterface

-- | Initialize the ApplicationMonad stack and enter the event loop.
runApplication :: (Show s, Model s, HasToken s, Show (TokenOf s)) => s -> IO ()
runApplication state =
    do  -- Initialize the application and get the initial state.
        appState <- setupApplication state
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

-- | Convert a `Timespec` to the `SimpleTime` (a double in seconds from application start)
toSimpleTime :: TimeSpec -> SimpleTime
toSimpleTime timeSpec = (fromIntegral . toNanoSecs $ timeSpec) / 1000000000

-- | Get the time elapsed from starting the event loop.
getElapsedTime :: ApplicationMonad s SimpleTime
getElapsedTime =
  do  startTime   <- use appStartTime
      currentTime <- liftIO $ getTime Realtime
      return $ toSimpleTime $ currentTime `diffTimeSpec` startTime

-- | Debug message from the ApplicationMonad
appMessage :: String -> ApplicationMonad s ()
appMessage = liftIO . putStrLn

-- | Call a function f in IO that uses the application state as it's first argument.
overState :: (MonadState g m) => Lens' g a -> (a -> m a) -> m ()
overState lens f =
  do  state <- use lens
      state' <- f state
      lens .= state'

-- | Initialize the timekeeper
restartAppTimer :: ApplicationMonad s ()
restartAppTimer = appTimeKeeper <~ liftIO startTimeKeeper

-- | First phase of event loop.
beginCycle :: ApplicationMonad s ()
beginCycle =
    do  restartAppTimer

-- | Update the model state and generate a shape tree, marking time along the way.
processState :: (Show s, Model s, HasToken s, Show (TokenOf s)) => SimpleTime -> [Input (TokenOf s)] -> ApplicationMonad s (Scene (ShapeTree (TokenOf s) SubSpace))
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        markAppTime "Advance State"
        state <- use appState
        let newState = updateModelState frame elapsedTime inputs state
        finalState <- liftIO (ioTask newState)
        appState .= finalState
        if null inputs
        then appMessage $ show finalState
        else appMessage $ show finalState ++ show inputs
        status <- use appStatus
        scene <- lift $ constructScene finalState status
        markAppTime "Build State"
        return scene

-- | Prepare and render the shapetree to a bitmap via the OpenCL kernel.
drawFrame :: (Model s, Show (TokenOf s)) => Int -> Scene (ShapeTree (TokenOf s) SubSpace) -> [(PointQueryId, Point2 SubSpace)] -> ApplicationMonad s (DrawTarget, [PointQueryResult (TokenOf s)])
drawFrame frameCount scene queries =
    do  --appMessage "ResetJob"
        rasterizer <- use appRasterizer
        target <- withIO appBackend (prepareTarget (rasterizer ^. rasterUseGLInterop))
        let canvasSize = P $ fromIntegral <$> target ^. targetArea
        state <- use appState
        pictureMap <- liftIO $ providePictureMap state
        markAppTime "Build TileTree"
        queryResults <- withSerializedScene rasterizer canvasSize pictureMap scene $
             \ pictDataPile serialState ->
                   do  markAppTime "Traverse ShapeTree"
                       -- | Create a specification for the current frame.
                       let rasterParams = RasterParams rasterizer
                                                       serialState
                                                       pictDataPile
                                                       queries
                                                       canvasSize
                                                       target
                                                       frameCount
                       appMessage "===================== rasterStart ====================="
                       queryResults <- liftIO $ runRaster rasterParams
                       appMessage "===================== rasterDone ====================="
                       markAppTime "Rasterize Threads"
                       --liftIO $ threadDelay 3000000
                       return queryResults
        return (target, toList queryResults)

-- Final phase of the event loop.
endCycle :: SimpleTime -> ApplicationMonad s ()
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
markAppTime :: String -> ApplicationMonad s ()
markAppTime message =
    do  tk  <- use appTimeKeeper
        tk' <- liftIO $ markTime tk message ()
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
loop :: (Show s, Model s, HasToken s, Show (TokenOf s)) => [Input (TokenOf s)] -> ApplicationMonad s ()
loop preppedInputs =
  do  --appMessage "checkInputs"
      unless (any isQuit preppedInputs) $
          do  elapsedTime <- getElapsedTime
              beginCycle
              scene <- processState elapsedTime preppedInputs
              frameCount <- fromIntegral <$> use appCycle
              newInputs <- withIO appBackend checkInputs
              let queries = pullQueries newInputs
              (target, queryResults) <- drawFrame frameCount scene queries
              let newPreppedInputs = tr "preppedInputs" $ attachQueryResults newInputs queryResults
              state      <- use appState
              state'     <- withIO appBackend $ handleOutput state target
              appState .= state'
              markAppTime "Raster Frame"
              endCycle elapsedTime
              liftIO performMinorGC -- the idea here is that if we perform garbage collection
                                    -- on each frame we'll get a more consistent frame rate.
              continue <- shouldLoop <$> use appState
              when continue (loop newPreppedInputs)
