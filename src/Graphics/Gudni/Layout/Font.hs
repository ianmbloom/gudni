{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE TypeSynonymInstances #-}


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
  , FontCache (..)
  , emptyFontCache
  , FontMonad (..)
  , runFontMonad
  , HasGlyph(..)
  , glyphString
  , addFont
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.ShapeTree

import Graphics.Gudni.Layout.Glyph

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import GHC.Float
import Data.Int
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Control.Lens
import Control.Monad.State
import Control.DeepSeq
import Data.Hashable

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Either

import qualified Graphics.Text.TrueType as F
import qualified Graphics.Text.TrueType.Internal as FI

-- | Wrapper newtype for codepoint values.
newtype CodePoint  = CodePoint  {unCodePoint  :: Int} deriving (Eq, Ord)

instance NFData CodePoint where
  rnf (CodePoint p) = rnf p

instance Hashable CodePoint where
  hashWithSalt s (CodePoint p) = s `hashWithSalt` p

instance Show CodePoint where
  show (CodePoint x) = "CodePoint "++show (chr x) ++ "->" ++ show x

-- | A cache of all glyphs that have been loaded from the font file so far and the font file itself.
data FontCache =
  FontCache
  { -- | Map from CodePoints to cached glyphs.
    _gCMap  :: M.Map CodePoint (Glyph (CompoundTree SubSpace))
    -- | Original font data structure. Contains an error message depending on how the font is loaded.
  , _gCFont :: Either String F.Font
  } deriving (Show)
makeLenses ''FontCache

-- | An initial 'FontCache'
emptyFontCache = FontCache M.empty (Left "No Font Loaded")

-- | Monad transformer for holding the glyphcache.
type FontMonad m = StateT FontCache m

-- | Evaluate a 'FontMonad'.
runFontMonad :: (Monad m) => FontMonad m a -> m a
runFontMonad mf = evalStateT mf emptyFontCache

instance HasSpace a => HasSpace (FontMonad m a) where
  type SpaceOf (FontMonad m a) = SpaceOf a

-- | Add a fontfile to the FontMonad.
addFont :: String -> FontMonad IO ()
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
getGlyph :: (MonadState FontCache m, Monad m) => CodePoint -> m (Glyph (CompoundTree SubSpace))
getGlyph codepoint =
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
                outlines :: [Outline SubSpace]
                outlines =  map (Outline . V.fromList . pairPoints . map ((^* fontScaleFactor) . fmap fromIntegral . pairToPoint)) vertices
                -- build the Glyph constructor including the metadata and the outlines.
                glyph :: Glyph (CompoundTree SubSpace)
                glyph = Glyph { _glyphBox = Box zeroPoint (Point2 (realToFrac advance * fontScaleFactor) (realToFrac (ascent + descent) * fontScaleFactor))
                              , _unGlyph  = SLeaf outlines
                              }
            in  do  -- insert the new glyph into the cache.
                    gCMap .= M.insert codepoint glyph dict
                    -- return it as well.
                    return glyph

class HasGlyph a where
  glyph :: (MonadState FontCache m, Monad m) => CodePoint -> m a

instance HasGlyph (CompoundTree SubSpace) where
  glyph codePoint = do g <- getGlyph codePoint
                       case g of
                           Glyph _ a  -> return a
                           EmptyGlyph -> return SEmpty

instance HasGlyph (Glyph (CompoundTree SubSpace)) where
  glyph = getGlyph

-- | Convert a string of characters to a list of glyphs in the FontMonad.
glyphString :: (MonadState FontCache m, Monad m, HasGlyph a)
            => String
            -> m [a]
glyphString = mapM (glyph . CodePoint . ord)
