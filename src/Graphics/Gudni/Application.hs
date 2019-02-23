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
import Graphics.Gudni.Raster.TileArray
import Graphics.Gudni.Raster.Job
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.RandomField

import System.Info

frameTimeMs = 3000000 --(3300000`div`10)*10 -- miliseconds

type SimpleTime = Float

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
  screenSize      :: s -> ScreenMode
  shouldLoop      :: s -> Bool
  fontFile        :: s -> IO String
  modelCursor     :: s -> Point2 IntSpace
  constructFigure :: Monad m => s -> String -> m (ShapeTreeRoot, String)
  updateModel     :: Monad m => Int -> SimpleTime -> [Input (Point2 IntSpace)] -> s -> m s
  pictureData     :: s -> IO (Maybe (Pile Word8), [PictureMemory])

type ApplicationMonad s = StateT (ApplicationState s) (TileArrayMonad IO)

runApplicationMonad = flip evalStateT

setupApplication :: Model s => s -> IO (ApplicationState s)
setupApplication state  =
  do  ----------- Setup OpenCL Kernels ----------------
      openCLLibrary <- setupOpenCL False False embeddedOpenCLSource
      ------------ Initialize Backend ---------------------
      backendState <- startInterface (screenSize state)
      ------------ Start TimeKeeper -------------------
      timeKeeper <- startTimeKeeper
      startTime <- getTime Realtime
      randomField <- makeRandomField rANDOMFIELDsIZE
      return $ AppState backendState timeKeeper startTime openCLLibrary "No Status" 0 state randomField

closeApplication :: ApplicationMonad s ()
closeApplication = withIO appBackend closeInterface

runApplication :: (Show s, Model s) => s -> IO ()
runApplication state =
    do  appState <- setupApplication state
        runGlyphMonad $
            do  mFontFile <- liftIO $ fontFile state
                addFont mFontFile
                runEnclosureMonad $
                    runTileArrayMonad $
                        runApplicationMonad appState $
                            do  loop
                                closeApplication

toSimpleTime :: TimeSpec -> SimpleTime
toSimpleTime timeSpec = (fromIntegral . toNanoSecs $ timeSpec) / 1000000000

getElapsedTime :: ApplicationMonad s SimpleTime
getElapsedTime =
  do  startTime   <- use appStartTime
      currentTime <- liftIO $ getTime Realtime
      return $ toSimpleTime $ currentTime `diffTimeSpec` startTime

appMessage :: String -> ApplicationMonad s ()
appMessage = liftIO . putStrLn

overJob :: MonadState s m1 => (m (a, s) -> m1 (b, s)) -> StateT s m a -> m1 b
overJob lifter mf =
    do  job <- get
        (a, job') <- lifter $ runStateT mf job
        put job'
        return a

overState f =
  do  state <- use appState
      state' <- liftIO $ f state
      appState .= state'

beginCycle :: ApplicationMonad s ()
beginCycle =
    do  --state <- use appState
        --stateInfo <- lift stateInfoText
        restartAppTimer

processState :: (Show s, Model s) => SimpleTime -> [Input (Point2 IntSpace)] -> ApplicationMonad s (ShapeTreeRoot, String)
processState elapsedTime inputs =
    do  frame <- fromIntegral <$> use appCycle
        overState $ updateModel frame elapsedTime inputs
        status <- use appStatus
        markAppTime "Advance State"
        state <- use appState
        if null inputs
        then appMessage $ show state
        else appMessage $ show state ++ show inputs

        --shapeTree <- lift . evalRandIO $ fuzz 5000
        (shapeTree, textForm) <- constructFigure state status
        --appMessage $ "ShapeTree " ++ show shapeTree
        --lift . putStrLn $ textForm
        markAppTime "Build State"
        return (shapeTree, "textForm")

fromJust (Just x) = x

drawFrame :: (Model s) => Point2 IntSpace -> CInt -> ShapeTreeRoot -> ApplicationMonad s ()
drawFrame cursor frame shapeTree =
    do  --appMessage "ResetJob"
        library <- use appOpenCLLibrary
        let openCLState = clState library
        target <- withIO appBackend (prepareTarget (clUseGLInterop library))
        let (V2 width height) = targetArea target
        lift $ resizeTileArray $ Point2 (fromIntegral width) (fromIntegral height)

        tileArray <- lift $ get
        let tileGrid = tileArray ^. tAGrid
        appS <- use appState
        (mPictData, mems) <- liftIO $ pictureData appS
        (shapes, primBag, blockShapes, shapeState) <- lift . lift  $ traverseShapeTree mems tileGrid shapeTree
        liftIO $ evaluate $ rnf (shapes, primBag, blockShapes, shapeState)
        markAppTime "Traverse Shape Tree"
        lift $ mapM_ addPrimBlock blockShapes
        markAppTime "Build Tile Array"
        randomField <- use appRandomField
        tileArray <- lift $ get
        let pictRefs = shapeState ^. stPictureRefs
            rasterParams = RasterParams library
                                        tileGrid
                                        target
                                        mPictData
                                        (shapeState ^. stPictureRefs)
                                        randomField
            jobInput = RasterJobInput (backgroundColor shapeTree)
                                      shapes
                                      primBag
                                      tileArray
        appMessage "===================== rasterStart ====================="
        lift $ buildAndQueueRasterJobs cursor frame rasterParams jobInput
        appMessage "===================== rasterDone ====================="
        markAppTime "Rasterize Threads"
        withIO appBackend $ presentTarget target
        lift $ resetTileArray
        --liftIO $ mapM freeJob jobs
        markAppTime "Raster Frame"

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

restartAppTimer :: ApplicationMonad s ()
restartAppTimer = appTimeKeeper <~ liftIO startTimeKeeper

markAppTime :: String -> ApplicationMonad s ()
markAppTime message =
    do  tk  <- use appTimeKeeper
        tk' <- liftIO $ markTime tk message ()
        appTimeKeeper .= tk'

isQuit :: Input a -> Bool
isQuit input =
    case input of
        InputWindow WindowClosed -> True
        InputKey Pressed (KeyModifier _ _ _ True) (KeyLetter LetterQ) -> True
        InputKey Pressed _ (KeyCommand CommandQuit) -> True
        _ -> False

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
              cursor <- modelCursor <$> use appState
              appMessage ("drawFrame " ++ show frame)
              drawFrame cursor frame shapeTree
              --appMessage "endCycle"
              endCycle elapsedTime
              liftIO performMinorGC
              continue <- shouldLoop <$> use appState
              when continue loop
