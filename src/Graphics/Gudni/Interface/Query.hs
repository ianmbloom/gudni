{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Interface.Query
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for queries, which are requests to the rasterizer to identify specific parts of
-- the rasterized scene

module Graphics.Gudni.Interface.Query
  ( PointQueryId(..)
  , PointQueryResult(..)
  , PointQuery(..)
  , pointQueryId
  , pointQueryPos
  , pullQueries
  , attachQueryResults
  )
where


import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure

import Graphics.Gudni.Util.StorableM

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import qualified Data.Sequence as S
import qualified Data.Map      as M
import Data.List.Lens
import Control.Lens
import Control.Lens.Indexed
import Data.Maybe

type PointQueryId_ = Int
newtype PointQueryId = PointQueryId {unPointQueryId :: PointQueryId_} deriving (Show, Eq, Ord, Num)
type PointQueryResult token = (PointQueryId,Maybe token)

data PointQuery s
    = PointQuery
    { _pointQueryId  :: PointQueryId
    , _pointQueryPos :: Point2 s
    }
makeLenses ''PointQuery


instance Storable PointQueryId where
  sizeOf (PointQueryId a) = sizeOf a
  alignment (PointQueryId a) = alignment a
  peek i = PointQueryId <$> peek (castPtr i)
  poke i (PointQueryId a) = poke (castPtr i) a

makeQuery :: Int -> Input token -> Maybe (PointQuery PixelSpace)
makeQuery i input =
  case (input ^. inputType) of
    InputMouse _ _ _ position -> Just (PointQuery (PointQueryId i) position)
    _ -> Nothing

pullQueries :: [Input token] -> [PointQuery PixelSpace]
pullQueries = catMaybes . imap (makeQuery)

attachQueryResults :: [Input token] -> [(PointQueryId, Maybe token)] -> [Input token]
attachQueryResults inputs queryResults = foldl attachResult inputs queryResults
  where
  attachResult inputs (PointQueryId i, token) = set (ix i . inputToken) token inputs

instance forall s . Storable s => StorableM (PointQuery s) where
    sizeOfM _ = do sizeOfM (undefined :: PointQueryId)
                   sizeOfM (undefined :: Point2 s)
    alignmentM _ = do alignmentM (undefined :: PointQueryId)
                      alignmentM (undefined :: Point2 s) -- filler
    peekM = do queryId  <- peekM
               queryPos <- peekM
               return (PointQuery queryId queryPos)
    pokeM (PointQuery queryId queryPos) =
            do pokeM queryId
               pokeM queryPos

instance Storable s => Storable (PointQuery s) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
