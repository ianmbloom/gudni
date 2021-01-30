{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveGeneric        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Glyph
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for accessing glyphs from a font file, storing them in a cache and converting them to outlines.

module Graphics.Gudni.Layout.Font
  ( CodePoint (..)
  , FontName (..)
  , Glyph (..)
  , FontCache (..)
  , emptyFontCache
  , FontMonad (..)
  , runFontMonad
  , addFont
  , getGlyph
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Proximity

import Graphics.Gudni.Util.CurvePair
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import GHC.Float
import Data.Int
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import Data.Hashable

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Either

import qualified Graphics.Text.TrueType as F
import qualified Graphics.Text.TrueType.Internal as FI


-- | Wrapper newtype for codepoint values.
newtype CodePoint  = CodePoint  {unCodePoint  :: Int} deriving (Eq, Ord, Generic)

instance Out CodePoint

type FontName = String

data Glyph = Glyph String CodePoint

instance Hashable CodePoint where
  hashWithSalt s (CodePoint p) = s `hashWithSalt` p

instance Show CodePoint where
  show (CodePoint x) = "CodePoint "++show (chr x) ++ "->" ++ show x

-- | A cache of all glyphs that have been loaded from the font file so far and the font file itself.
data FontCache s =
  FontCache
  { -- | Map from CodePoints to cached glyphs.
    _gCMap  :: M.Map CodePoint (WithBox (Shape s))
    -- | Original font data structure. Contains an error message depending on how the font is loaded.
  , _gCFont :: Either String F.Font
  }
makeLenses ''FontCache

-- | An initial 'FontCache'
emptyFontCache = FontCache M.empty (Left "No Font Loaded")

-- | Monad transformer for holding the glyphcache.
type FontMonad s m = StateT (FontCache s) m

-- | Evaluate a 'FontMonad'.
runFontMonad :: (Monad m) => FontMonad s m a -> m a
runFontMonad mf = evalStateT mf emptyFontCache

-- instance (HasSpace a, s~SpaceOf a) => HasSpace (FontMonad s m a) where
--   type SpaceOf (FontMonad s m a) = SpaceOf a

-- | Add a fontfile to the FontMonad.
addFont :: String -> FontMonad s IO ()
addFont file_name =
  do  -- load the TrueType font file into a buffer.
      ttf_buffer <- liftIO $ LB.readFile file_name
      -- Decode it into the glyph monad cache.
      gCFont .= F.decodeFont ttf_buffer

-- | Helper function for translating font file pairs to points.
pairToPoint (x,y) = Point2 x y

-- | Helper function to flip the data inside glyphs
flipY h (x,y) = (x, h + negate y)

-- | Accessors into font internals.
--makeLenses ''F.Font
--makeLenses ''FI.HorizontalHeader
--makeLenses ''F.RawGlyph

rightOrError (Right t) = t
rightOrError (Left err) = error err

-- | Retrieve a glyph from the glyphCache, read it from the font file if necessary.
getGlyph :: forall s m .
            ( Monad m
            , Space s
            , MonadState (FontCache s) m
            )
         => String
         -> CodePoint
         -> m (WithBox (Shape s))
getGlyph fontName_ codepoint =
  do  -- the current map from codepoints to previously decoded glyphs.
      dict <- use gCMap
      -- the original font data structure
      eFont <- use gCFont
      -- check if the glyph has already been loaded.
      case M.lookup codepoint dict of
          Just glyph -> return $ glyph -- if so return it.
          Nothing ->
            let -- otherwise load it from the font, store it in the cache and return it.
                font = rightOrError eFont
                -- get the scale factor from the font.
                fontScaleFactor :: s
                fontScaleFactor = 1 / (fromIntegral $ F.unitsPerEm font)
                -- get the width to advance for this glyph and the vector of RawGlyph structures.
                glyphVector :: V.Vector F.RawGlyph
                (advance, glyphVector) = F.getCharacterGlyphsAndMetrics font (chr . unCodePoint $ codepoint)
                -- get the general metadata header for the font.
                header = font ^. F.fontHorizontalHeader
                -- pull the acent and descent information for the header.
                ascent  = (fromJust header) ^. FI.hheaAscent
                descent = (fromJust header) ^. FI.hheaDescent
                -- get the list of contours from first rawglyph.
                contours :: [VU.Vector (Int16, Int16)]
                contours = if V.length glyphVector > 0
                           then (V.head glyphVector) ^. F.rawGlyphContour
                           else []
                height = ascent + descent
                vertices :: [[(Int16,Int16)]]
                -- turn the contours to lists of pairs and flip them vertically.
                vertices = map (map (flipY (fromIntegral $ height)) . VU.toList) contours
                -- create outlines from the lists of vertices by converting them to Point2 SubSpace and scaling them
                -- by the fontfactor, this means a normal glyph will have a height of 1 in SubSpace.
                shape :: Shape s
                shape =  Shape . map (Outline . pairsToBeziers . V.fromList . pairPoints . map ((^* (realToFrac fontScaleFactor)) . fmap fromIntegral . pairToPoint)) $ vertices
                -- build the Glyph constructor including the metadata and the outlines.
                glyphBox :: Box s
                glyphBox = Box zeroPoint (Point2 (realToFrac advance * realToFrac fontScaleFactor) (realToFrac (ascent + descent) * realToFrac fontScaleFactor))
                glyph :: WithBox (Shape s)
                glyph = WithBox shape glyphBox
            in  do  -- insert the new glyph into the cache.
                    gCMap .= M.insert codepoint glyph dict
                    -- return it as well.
                    return glyph
