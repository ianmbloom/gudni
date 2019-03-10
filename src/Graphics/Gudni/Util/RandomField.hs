-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.RandomField
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for creating a buffer of random floats.

module Graphics.Gudni.Util.RandomField
  ( RandomField(..)
  , makeRandomField
  )
where

import System.Random
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CFloat)

-- | Random field is a static vector of random floats used by the rasterizer to do stochastic antialiasing.
type RandomField = VS.Vector CFloat

-- | Create a vector of random floats between 0 and 1.
makeRandomField :: Int -> IO RandomField
makeRandomField size =
  do g <- getStdGen
     return $ VS.fromList $ take size (randomRs (0.0,1.0) g)
