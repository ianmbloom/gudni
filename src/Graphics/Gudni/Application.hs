{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}
module Graphics.Gudni.Application
  ( runApplication
  , Model(..)
  , ScreenMode(..)
  , SimpleTime
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

import Graphics.Gudni.OpenCL.Setup
import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.CallKernels

import Graphics.Gudni.Raster.Types

import Graphics.Gudni.Raster.Constants (rANDOMFIELDsIZE)
import Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Geometry
import Graphics.Gudni.Raster.Job
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.RandomField

import System.Info

frameTimeMs = 3000000 --(3300000`div`10)*10 -- miliseconds

type SimpleTime = Double

data ApplicationState s = AppState
    { _appBackend       :: InterfaceState
    , _appTimeKeeper    :: TimeKeeper
    , _appStartTime     :: TimeSpec
    , _appOpenCLLibrary :: OpenCLKernelLibrary
    , _appStatus        :: String
    , _appCycle         :: Int
    , _appState         :: s
    , _appRandomField   :: RandomField -- can't find a better place to put this.
    }
makeLenses ''ApplicationState

class Model s where
  -- | Construct a ShapeTreeRoot from the state of type `s`
  constructFigure  :: s -> String -> GlyphMonad IO (ShapeTreeRoot, String)
  -- | Update the state based on the elapsed time and a list of inputs
  updateModelState :: Monad m => Int -> SimpleTime -> [Input (Point2 IntSpace)] -> s -> m s
  -- | Set the initial display to FullScreen or a specific window size in pixels.
  screenSize       :: s -> ScreenMode
  -- | Determine if the application will enter the event loop.
  -- for debugging purposes you can set this to False and render on frame and quit.
  shouldLoop       :: s -> Bool
   -- | Path to the Truetype font file that is initially loaded.
  fontFile         :: s -> IO String
  -- | Bitmap texture data provided from the state for the rendered scene.
  providePictureData :: s -> IO (Maybe (Pile Word8), [PictureMemory])

-- | Monad Stack for the event loop.
type ApplicationMonad s = StateT (ApplicationState s) (EnclosureMonad (GlyphMonad IO))

runApplicationMonad = flip evalStateT

-- | Initializes openCL, frontend interface, timekeeper, randomfield data and returns the initial `ApplicationState`
setupApplication :: Model s => s -> IO (ApplicationState s)
setupApplication state  =
  do  ----------- Setup OpenCL Kernels ----------------
      openCLLibrary <- setupOpenCL False False openCLSourceWithDefines
      ------------ Initialize Backend ---------------------
      backendState <- startInterface (screenSize state)
      ------------ Start TimeKeeper -------------------
      timeKeeper <- startTimeKeeper
      startTime <- getTime Realtime
      randomField <- makeRandomField rANDOMFIELDsIZE
      return $ AppState backendState timeKeeper startTime openCLLibrary "No Status" 0 state randomField

-- | Closes the interface.
closeApplication :: ApplicationMonad s ()
closeApplication = withIO appBackend closeInterface

-- | Initialize the ApplicationMonad stack and enter the event loop.
runApplication :: (Show s, Model s) => s -> IO ()
runApplication state =
    do  appState <- setupApplication state
        runGlyphMonad $
            do  mFontFile <- liftIO $ fontFile state
                addFont mFontFile
                runEnclosureMonad $
                    runApplicationMonad appState $
                        do  loop
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
overState f =
  do  state <- use appState
      state' <- liftIO $ f state
      appState .= state'

-- | Initialize the timekeeper
restartAppTimer :: ApplicationMonad s ()
restartAppTimer = appTimeKeeper <~ liftIO startTimeKeeper

-- | First phase of event loop.
beginCycle :: ApplicationMonad s ()
beginCycle =
    do  restartAppTimer

-- | Update the model state and generate a shape tree, marking time along the way.
processState :: (Show s, Model s) => SimpleTime -> [Input (Point2 IntSpace)] -> ApplicationMonad s (ShapeTreeRoot, String)
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        overState $ updateModelState frame elapsedTime inputs
        status <- use appStatus
        markAppTime "Advance State"
        state <- use appState
        if null inputs
        then appMessage $ show state
        else appMessage $ show state ++ show inputs

        --shapeTree <- lift . evalRandIO $ fuzz 5000
        (shapeTree, textForm) <- lift . lift $ constructFigure state status
        --appMessage $ "ShapeTree " ++ show shapeTree
        --lift . putStrLn $ textForm
        markAppTime "Build State"
        return (shapeTree, "textForm")

-- | Prepare and render the shapetree to a bitmap via the OpenCL kernel.
drawFrame :: (Model s) => CInt -> ShapeTreeRoot -> ApplicationMonad s ()
drawFrame frame shapeTree =
    do  --appMessage "ResetJob"
        library <- use appOpenCLLibrary
        let openCLState = clState library
        target <- withIO appBackend (prepareTarget (clUseGLInterop library))
        let (V2 width height) = targetArea target
        let canvasSize = Point2 width height
        appS <- use appState
        (mPictData, mems) <- liftIO $ providePictureData appS
        (substances, boundedShapedEnclosures, shapeState) <- lift  $ traverseShapeTree mems (fromIntegral <$> canvasSize) shapeTree
        (shapeEntries, geometryPile) <- liftIO $ buildGeometryPile $ concat boundedShapedEnclosures
        --liftIO $ evaluate $ rnf (substances, boundedShapedEnclosures, shapeState)
        markAppTime "Traverse Shape Tree"
        let tileTree = buildTileTree (fromIntegral <$> canvasSize)
            tileTree' = {-tr "tree" $-} foldl addShapeToTree tileTree {-$ tr "boxShapes"-} shapeEntries
        --liftIO $ putStrLn $ show tileTree'
        markAppTime "Build Tile Array"
        randomField <- use appRandomField
        let pictRefs = shapeState ^. stPictureRefs
            rasterParams = RasterParams library
                                        target
                                        geometryPile
                                        mPictData
                                        (shapeState ^. stPictureRefs)
                                        randomField
            jobInput = RasterJobInput (backgroundColor shapeTree)
                                      substances
                                      tileTree'
        appMessage "===================== rasterStart ====================="
        liftIO $ buildAndQueueRasterJobs frame rasterParams jobInput tileTree'
        appMessage "===================== rasterDone ====================="
        markAppTime "Rasterize Threads"
        withIO appBackend $ presentTarget target
        markAppTime "Raster Frame"
        --geometryPile %= resetPile
        liftIO $ freePile geometryPile
        --liftIO $ threadDelay 3000000

-- Final phase of the event loop.
endCycle :: SimpleTime -> ApplicationMonad s ()
endCycle elapsedTime =
    do  tk     <- use appTimeKeeper
        cycleCount <- use appCycle
        let status = showTimes "Loop Cycle" True tk
                   -- ++ show job
                   ++ "------ Cycle: "
                   ++ show cycleCount
                   ++ "\n ------ Elapsed Time: "
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
        InputKey Pressed (KeyModifier _ _ _ True) (KeyLetter LetterQ) -> True
        InputKey Pressed _ (KeyCommand CommandQuit) -> True
        _ -> False

-- | Cycle through the event loop.
loop :: (Show s, Model s) => ApplicationMonad s ()
loop  =
  do  --appMessage "checkInputs"
      inputs <- withIO appBackend checkInputs
      unless (any isQuit inputs) $
          do  elapsedTime <- getElapsedTime
              --appMessage "beginCycle"
              beginCycle
              --appMessage "processState"
              (shapeTree, textForm) <- processState elapsedTime inputs
              frame <- fromIntegral <$> use appCycle
              appMessage ("drawFrame " ++ show frame)
              drawFrame frame shapeTree
              --appMessage "endCycle"
              endCycle elapsedTime
              liftIO performMinorGC
              continue <- shouldLoop <$> use appState
              when continue loop
