{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Gudni.Raster.Dag.State
   ( Tile(..)
   , DagState(..)
   , dagFabricTokenMap
   , dagStorage
   , dagRasterizer
   , dagBitmapSize
   , dagTile
   , dagDrawTarget
   , dagFrameCount

   , outputDagState
   )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Interface.DrawTarget

import Control.Lens
import qualified Data.Map as M
import Foreign.Storable

type Tile = Box PixelSpace

-- | Constructor for holding the state of serializing substance information from the scene.
data DagState r token s = DagState
    { -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
      _dagFabricTokenMap :: M.Map FabricTagId token
      -- | All the piles of dag information.
    , _dagStorage        :: DagStorage s
      -- |
    , _dagRasterizer     :: r
      -- |
    , _dagBitmapSize     :: Point2 PixelSpace
    , _dagTile           :: Tile
    , _dagDrawTarget     :: DrawTarget
    , _dagFrameCount     :: Int
    }
makeLenses '' DagState

outputDagState :: ( Show s
                  , Show token
                  , Storable s
                  )
               => DagState r token s
               -> IO ()
outputDagState state =
  do  putStrLn $ "dagTokenMap         " ++ (show . view dagFabricTokenMap  $ state)
