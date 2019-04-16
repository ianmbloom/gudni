{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExplicitForAll       #-}

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

module Graphics.Gudni.Figure.Glyph
  ( CodePoint (..)
  , Glyph (..)
  , mapGlyph
  , glyphBox
  , glyphRep
  , GlyphCache (..)
  , emptyGlyphCache
  , GlyphMonad (..)
  , runGlyphMonad
  , getGlyph
  , addFont
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.ShapeTree

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

-- | Glyph data structure
data Glyph a =
  Glyph
  { -- | A user defined box that determines how the glyph can be juxtaposed with other glyphs.
    _glyphBox       :: Point2 (SpaceOf a)
  -- | The actual representation of the glyph.
  , _glyphRep        :: a
  }
makeLenses ''Glyph

deriving instance (Show (SpaceOf a), Show a) => Show (Glyph a)
deriving instance (Eq   (SpaceOf a), Eq   a) => Eq   (Glyph a)
deriving instance (Ord  (SpaceOf a), Ord  a) => Ord  (Glyph a)

mapGlyph :: forall a b . (SpaceOf a ~ SpaceOf b) => (a->b) -> Glyph a -> Glyph b
mapGlyph f (Glyph box a) = Glyph box (f a)

instance SimpleTransformable a => SimpleTransformable (Glyph a) where
  tTranslate p = mapGlyph (tTranslate p)
  tScale f (Glyph box a) = Glyph (tScale f box) (tScale f a)

instance HasSpace a => HasSpace (Glyph a) where
  type SpaceOf (Glyph a) = SpaceOf a

instance (Hashable a, Hashable (SpaceOf a)) => Hashable (Glyph a) where
    hashWithSalt s (Glyph a b) = s `hashWithSalt` a `hashWithSalt` b

instance (NFData a, NFData (SpaceOf a)) => NFData (Glyph a) where
  rnf (Glyph a b) = a `deepseq` b `deepseq` ()

-- | A cache of all glyphs that have been loaded from the font file so far and the font file itself.
data GlyphCache =
  GlyphCache
  { -- | Map from CodePoints to cached glyphs.
    _gCMap  :: M.Map CodePoint (Glyph (CompoundTree SubSpace))
    -- | Original font data structure. Contains an error message depending on how the font is loaded.
  , _gCFont :: Either String F.Font
  } deriving (Show)
makeLenses ''GlyphCache

-- | An initial 'GlyphCache'
emptyGlyphCache = GlyphCache M.empty (Left "No Font Loaded")

-- | Monad transformer for holding the glyphcache.
type GlyphMonad m = StateT GlyphCache m

-- | Evaluate a 'GlyphMonad'.
runGlyphMonad :: (Monad m) => GlyphMonad m a -> m a
runGlyphMonad mf = evalStateT mf emptyGlyphCache

-- | Add a fontfile to the GlyphMonad.
addFont :: String -> GlyphMonad IO ()
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
getGlyph :: (MonadState GlyphCache m, Monad m) => CodePoint -> m (Glyph (CompoundTree SubSpace))
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
                glyph = Glyph { _glyphBox     = Point2 (realToFrac advance * fontScaleFactor)
                                                       (realToFrac (ascent + descent) * fontScaleFactor)
                              , _glyphRep     = SLeaf outlines
                              }
            in  do  -- insert the new glyph into the cache.
                    gCMap .= M.insert codepoint glyph dict
                    -- return it as well.
                    return glyph
