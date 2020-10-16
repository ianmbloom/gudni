{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
  , pullQueries
  , attachQueryResults
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure

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

instance Storable PointQueryId where
  sizeOf (PointQueryId a) = sizeOf a
  alignment (PointQueryId a) = alignment a
  peek i = PointQueryId <$> peek (castPtr i)
  poke i (PointQueryId a) = poke (castPtr i) a

makeQuery :: Int -> Input token -> Maybe (PointQueryId, Point2 SubSpace)
makeQuery i input =
  case (input ^. inputType) of
    InputMouse _ _ _ position -> Just (PointQueryId i, fmap fromIntegral position)
    _ -> Nothing

pullQueries :: [Input token] -> [(PointQueryId, Point2 SubSpace)]
pullQueries = catMaybes . imap (makeQuery)

attachQueryResults :: [Input token] -> [(PointQueryId, Maybe token)] -> [Input token]
attachQueryResults inputs queryResults = foldl attachResult inputs queryResults
  where
  attachResult inputs (PointQueryId i, token) = set (ix i . inputToken) token inputs
