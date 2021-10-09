{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.Raster.OpenCL.Util.Buffer
  ( newBuffer
  , releaseBuffer
  , pileToBuffer
  , bufferFromPile
  , bufferFromVector
  )
where

import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Util.Debug

import qualified Data.Vector.Storable as VS

import CLUtil.KernelArgs
import CLUtil.VectorBuffers
import CLUtil

import Foreign.Storable
import Foreign.Ptr

newBuffer :: (Storable a) => String -> Int -> CL (CLBuffer a)
newBuffer message size =
  do buffer <- allocBuffer [CL_MEM_READ_WRITE] (max 1 size)
     --liftIO $ putStrLn $ "   newBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
     return buffer

releaseBuffer :: String -> CLBuffer a -> CL Bool
releaseBuffer message buffer =
  do result <- liftIO $ clReleaseMemObject . bufferObject $ buffer
     --liftIO $ putStrLn $ "releaseBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
     return result

-- | Convert a pile to an OpenCL memory buffer.
pileToBuffer :: forall t . (Storable t) => CLContext -> Pile t -> IO (CLBuffer t)
pileToBuffer context (Pile cursor _ startPtr) =
    let vecSize = fromIntegral cursor * sizeOf (undefined :: t)
        adjustedVecSize = max 1 vecSize -- OpenCL will reject a memory buffer with size 0 so the minimum size is 1.
    in  CLBuffer vecSize <$> clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (adjustedVecSize, castPtr startPtr)

-- | Convert a pile to an OpenCL memory buffer.
pileToBufferMes :: forall t . (Storable t) => String -> CLContext -> Pile t -> IO (CLBuffer t)
pileToBufferMes message context (Pile cursor _ startPtr) =
    let vecSize = (tr (message ++ " cursor") $ fromIntegral cursor) * (tr (message ++ " sizeOf t") $ sizeOf (undefined :: t))
        adjustedVecSize = tr (message ++ "adjustedVecSize") $ max 1 vecSize -- OpenCL will reject a memory buffer with size 0 so the minimum size is 1.
    in  CLBuffer vecSize <$> clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (adjustedVecSize, castPtr startPtr)

bufferFromPile :: (Storable a) => String -> Pile a -> CL (CLBuffer a)
bufferFromPile message pile =
   do context <- clContext <$> ask
      buffer <- liftIO $ pileToBufferMes message context pile
      --liftIO $ putStrLn $ "  pileBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
      return buffer

bufferFromVector :: (Storable a) => String -> VS.Vector a -> CL (CLBuffer a)
bufferFromVector message vector =
   do context <- clContext <$> ask
      buffer <- liftIO $ vectorToBuffer context vector
      --liftIO $ putStrLn $ "vectorBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
      return buffer
