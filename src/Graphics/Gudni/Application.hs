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

import Graphics.Gudni.OpenCL.Setup
import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.CallKernels


import Graphics.Gudni.Raster.Constants (rANDOMFIELDsIZE)
import Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.Job
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.RandomField

import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as S
import System.Info

-- | The model typeclass is the primary interface to the application functions in Gudni
class Model s where
  -- | Construct a Scene from the state of type `s`
  constructScene  :: s -> String -> FontMonad IO (Scene (ShapeTree Int SubSpace))
  -- | Update the state based on the elapsed time and a list of inputs
  updateModelState :: Int -> SimpleTime -> [Input (Point2 PixelSpace)] -> s -> s
  -- | Do tasks in the IO monad based and update the current state.
  ioTask           :: MonadIO m => s -> m s
  -- | Set the initial display to FullScreen or a specific window size in pixels.
  screenSize       :: s -> ScreenMode
  -- | Determine if the application will enter the event loop.
  -- for debugging purposes you can set this to False and render one frame and quit.
  shouldLoop       :: s -> Bool
  -- | Path to the Truetype font file that is initially loaded.
  fontFile         :: s -> IO String
  -- | Bitmap texture data provided from the state for the rendered scene.
  providePictureMap :: s -> IO PictureMap
  -- | Do something with the output of the rasterizer.
  handleOutput :: s -> DrawTarget -> StateT InterfaceState IO s

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
type ApplicationMonad s = StateT (ApplicationState s) (GeometryMonad (FontMonad IO))

runApplicationMonad :: ApplicationState s -> ApplicationMonad s a -> GeometryMonad (FontMonad IO) a
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
runApplication :: (Show s, Model s) => s -> IO ()
runApplication state =
    do  -- Initialize the application and get the initial state.
        appState <- setupApplication state
        -- Start the glyph monad.
        runFontMonad $
            do  -- Load a font file.
                mFontFile <- liftIO $ fontFile state
                -- Add the font file to the glyph monad.
                addFont mFontFile
                -- Generate a random field for the stochastic aliasing of the rasterizer.
                randomField <- liftIO $ makeRandomField rANDOMFIELDsIZE
                -- Run the geometry serialization monad.
                runGeometryMonad (appState ^. appRasterizer . rasterSpec) randomField $
                    -- Run the application monad.
                    runApplicationMonad appState $
                        do  -- start the event loop.
                            loop
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
processState :: (Show s, Model s) => SimpleTime -> [Input (Point2 PixelSpace)] -> ApplicationMonad s (Scene (ShapeTree Int SubSpace))
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        markAppTime "Advance State"
        state <- use appState
        appState .= updateModelState frame elapsedTime inputs state
        state <- use appState
        state' <- liftIO (ioTask state)
        appState .= state'
        if null inputs
        then appMessage $ show state
        else appMessage $ show state ++ show inputs
        status <- use appStatus
        shapeTree <- lift . lift $ constructScene state status
        markAppTime "Build State"
        return shapeTree

-- | Prepare and render the shapetree to a bitmap via the OpenCL kernel.
drawFrame :: (Model s) => CInt -> Scene (ShapeTree Int SubSpace) -> ApplicationMonad s DrawTarget
drawFrame frameCount scene =
    do  --appMessage "ResetJob"
        rasterizer <- use appRasterizer
        target <- withIO appBackend (prepareTarget (rasterizer ^. rasterUseGLInterop))
        state <- use appState
        pictureMap <- liftIO $ providePictureMap state
        let canvasSize = P (targetArea target)
        lift (geoCanvasSize .= (fromIntegral <$> canvasSize))
        let maxTileSize = rasterizer ^. rasterSpec . specMaxTileSize
        lift (geoTileTree .= buildTileTree S.empty maxTileSize (fromIntegral <$> canvasSize))
        markAppTime "Build TileTree"
        (scenePictMem, pictDataPile) <- liftIO $ assignScenePictureMemory pictureMap scene
        substanceState <- lift ( execSubstanceMonad $ buildOverScene scenePictMem)
        --liftIO $ evaluate $ rnf (substances, boundedShapedEnclosures, substanceState)
        markAppTime "Traverse ShapeTree"
        geometryState <- lift $ get
        --liftIO $ putStrLn $ "TileTree " ++ show (geometryState ^. geoTileTree)
        let rasterParams = RasterParams rasterizer
                                        target
                                        geometryState
                                        substanceState
                                        pictDataPile
        appMessage "===================== rasterStart ====================="
        jobs <- lift $ buildRasterJobs rasterParams
        markAppTime "Build Raster Jobs"
        lift $ queueRasterJobs frameCount rasterParams jobs
        appMessage "===================== rasterDone ====================="
        markAppTime "Rasterize Threads"
        lift resetGeometryMonad
        liftIO $ freeRasterJobs jobs
        --liftIO $ threadDelay 3000000
        return target

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
isQuit :: Input a -> Bool
isQuit input =
    case input of
        InputWindow WindowClosed -> True
        InputKey Pressed (KeyModifier _ _ _ True) (Key LetterQ) -> True
        InputKey Pressed _ (KeyCommand CommandQuit) -> True
        _ -> False

-- | Cycle through the event loop.
loop :: (Show s, Model s) => ApplicationMonad s ()
loop  =
  do  --appMessage "checkInputs"
      inputs <- withIO appBackend checkInputs
      unless (any isQuit inputs) $
          do  elapsedTime <- getElapsedTime
              beginCycle
              scene      <- processState elapsedTime inputs
              frameCount <- fromIntegral <$> use appCycle
              target     <- drawFrame frameCount scene
              state      <- use appState
              state'     <- withIO appBackend $ handleOutput state target
              appState .= state'
              markAppTime "Raster Frame"
              endCycle elapsedTime
              liftIO performMinorGC -- the idea here is that if we perform garbage collection
                                    -- on each frame we'll get a more consistent frame rate.
              continue <- shouldLoop <$> use appState
              when continue loop
