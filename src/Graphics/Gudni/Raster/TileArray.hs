{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}

module Graphics.Gudni.Raster.TileArray
( TileArray (..)
, TileArrayMonad (..)
, runTileArrayMonad
--, liftTileArrayMonad
, showTileArray
, tAGrid
, newTileArray
, freeTileArray
, resizeTileArray
, tileArrayBounds
, tileArrayIndices
, addPrimBlock
, readTile
, resetTileArray
)
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Primitive

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile


import Data.Array.IO
import Data.Foldable

import Foreign.Storable

import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Control.Loop (numLoop)
import Control.Lens

----------------------- Job Leaf -----------------------

data Tile = Tile
    { tilePrims  :: Pile PrimId
    }

data TileArray = TileArray
  { _tArr   :: IOArray Int Tile
  , _tAGrid :: TileGrid
  }
makeLenses ''TileArray

type TileArrayMonad m = StateT TileArray (EnclosureMonad m)

runTileArrayMonad :: MonadIO m => TileArrayMonad m () -> EnclosureMonad m ()
runTileArrayMonad code = do tileArray <- liftIO $ newTileArray tILEsIZE zeroPoint
                            runStateT code tileArray
                            liftIO $ freeTileArray tileArray

--liftTileArrayMonad :: Monad m => m a -> TileArrayMonad m a
--liftTileArrayMonad = lift . lift . lift . lift . lift

showTile :: Int -> Tile -> IO ()
showTile i tile =
  do prims  <- pileToList $ tilePrims tile
     putStrLn $ show i ++ ": Prims: " ++ show prims

showTileArray :: TileArray -> IO ()
showTileArray tileArray =
  do assocs <- getAssocs $ tileArray ^. tArr
     putStrLn "---------------- TileArray ---------------- "
     forM_ assocs $ \ (i, e) -> showTile i e

tileArrayBounds :: TileArray -> IO (Int, Int)
tileArrayBounds tileArray = getBounds $ tileArray ^. tArr

tileArrayIndices :: TileArray -> IO [Int]
tileArrayIndices tileArray =
  do (i,j) <- getBounds $ tileArray ^. tArr
     return [i..j]

newTile :: IO Tile
newTile = do primPile  <- newPileSize dEFAULTNUMMASKS
             return $ Tile primPile


newTileGrid tileSize screenSize =
    let toInt = convert :: DisplaySpace -> IntSpace
        gridSizeX  = (fmap toInt (pX screenSize) + pX tileSize - 1) `div` pX tileSize
        gridSizeY  = (fmap toInt (pY screenSize) + pY tileSize - 1) `div` pY tileSize
    in  TileGrid { _tGScreenSize = screenSize
                 , _tGTileSize   = (convert :: Point2 IntSpace -> Point2 DisplaySpace) tileSize
                 , _tGGridSize   = makePoint gridSizeX gridSizeY
                 }

getArraySize tileGrid = let gridSize = tileGrid ^. tGGridSize
                        in  fromIntegral (pX gridSize * orthoganal (pY gridSize))

newTileArray :: Point2 IntSpace
             -> Point2 DisplaySpace
             -> IO TileArray
newTileArray tileSize screenSize =
  let tileGrid  = newTileGrid tileSize screenSize
      arraySize = getArraySize tileGrid
  in
  do  tileList <- replicateM arraySize newTile
      arr <- newListArray (0, arraySize - 1) tileList
      return TileArray { _tArr   = arr
                       , _tAGrid = tileGrid
                       }

resizeTileArray :: Point2 DisplaySpace -> TileArrayMonad IO ()
resizeTileArray displaySize =
  do  tileArray <- get
      let oldDisplaySize = tileArray ^. tAGrid . tGScreenSize
      if displaySize /= oldDisplaySize
      then do liftIO $ freeTileArray tileArray
              put =<< liftIO (newTileArray tILEsIZE displaySize :: IO TileArray)
      else return ()

freeTile :: Tile -> IO ()
freeTile (Tile prims) = freePile prims

freeTileArray :: TileArray -> IO ()
freeTileArray tileArray = mapM_ (\ i -> do tile <- readArray (tileArray ^. tArr) i
                                           freeTile tile
                                  ) =<< tileArrayIndices tileArray

----------------------- Add Strand To JobLeaf -------------------
-- This is the slower version where each strand is added to each tile that it overlaps
-- By adding each one to the leafCurrentSlices. Then the shape is added to


{-# INLINE findTile #-}
findTile :: Ortho XDimension IntSpace -> Ortho YDimension IntSpace -> Ortho XDimension IntSpace -> Int
findTile offsetGridX offsetGridY gridSizeX = fromIntegral $ unOrtho offsetGridX + unOrtho offsetGridY * unOrtho gridSizeX

addPrimToTileArray :: Ortho XDimension IntSpace
                   -> PrimId
                   -> Ortho YDimension IntSpace
                   -> Ortho XDimension IntSpace
                   -> TileArrayMonad IO ()
addPrimToTileArray gridSizeX primId offsetGridY offsetGridX =
  do tileArray <- get
     -- get the tile index
     let i = findTile offsetGridX offsetGridY gridSizeX
     -- get the existing tile pile
     liftIO $ do tile <- readArray (tileArray ^. tArr) i
                 -- add the shape reference to the pile for the tile
                 (tilePrims',ref) <- addToPile "tileShapePile" (tilePrims tile) primId
                 -- write the modified tile back to the array
                 writeArray (tileArray ^. tArr) i (Tile tilePrims')

{-# INLINE addPrimBlock #-}
addPrimBlock :: (Block, PrimId) -> TileArrayMonad IO ()
addPrimBlock (block, primId) =
  -- iterate over all of the tiles covered by the block
    do  tileArray <- get
        let gridSizeX = pX $ tileArray ^. tAGrid . tGGridSize
            iterateX y = numLoop (leftBox block) (rightBox block ) (addPrimToTileArray gridSizeX primId y)
        numLoop (topBox block) (bottomBox block) iterateX
        put tileArray

readTile :: MonadIO m => TileArray -> Int ->m [PrimId]
readTile array index =
    do liftIO $ do tile <- readArray (array ^. tArr) index
                   pileToList (tilePrims tile)

resetTile :: Int -> TileArrayMonad IO ()
resetTile index =
    do array <- use tArr
       liftIO $ do (Tile prims) <- readArray array index
                   let prims'  = resetPile prims
                   writeArray array index (Tile prims')

resetTileArray :: TileArrayMonad IO ()
resetTileArray = do indices <- tileArrayIndices <$> get
                    traverse_ resetTile =<< liftIO indices
