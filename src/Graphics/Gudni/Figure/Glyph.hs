{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

module Graphics.Gudni.Figure.Glyph
  ( CodePoint (..)
  , Glyph (..)
  , glyphHeight
  , GlyphCache (..)
  , emptyGlyphCache
  , GlyphMonad (..)
  , runGlyphMonad
  , getGlyph
  , glyphString
  , addFont
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Outline

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
  { -- | Series of outlines for the glyph.
    glyphVertices        :: [Outline a]
    -- | The advance width of the glyph according to the font file.
  , glyphAdvanceWidth    :: Ortho XDimension a
    -- | The ascent height of the glyph according to the font file.
  , glyphAscent          :: Ortho YDimension a
    -- | The descent height of the glyph according to the font file.
  , glyphDescent         :: Ortho YDimension a
  } deriving (Show, Eq, Ord)

-- | The total height of the glyph according to the font file.
glyphHeight glyph = glyphAscent glyph + glyphDescent glyph

instance Hashable a => Hashable (Glyph a) where
    hashWithSalt s (Glyph  a b c d) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d

instance NFData a => NFData (Glyph a) where
  rnf (Glyph a b c d ) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

-- | A cache of all glyphs that have been loaded from the font file so far and the font file itself.
data GlyphCache =
  GlyphCache
  { _gCMap  :: M.Map CodePoint (Glyph DisplaySpace)
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


fromRight (Right x) = x
fromRight (Left message) = error message

-- | Add a fontfile to the GlyphMonad.
addFont :: String -> GlyphMonad IO ()
addFont file_name =
  do
    ttf_buffer <- liftIO $ LB.readFile file_name
    gCFont .= F.decodeFont ttf_buffer

-- Helper function for translating fontfiles.
pairToPoint (x,y) = Point2 x y

flipY h (x,y) = (x, h + negate y)

makeLenses ''F.Font
makeLenses ''FI.HorizontalHeader
makeLenses ''F.RawGlyph

-- | Retrieve a glyph from the glyphCache, read it from the font file if necessary.
getGlyph :: (MonadState GlyphCache m, Monad m) => CodePoint -> m (Glyph DisplaySpace)
getGlyph codepoint =
  do  dict <- use gCMap
      eFont <- use gCFont
      case M.lookup codepoint dict of
          Just glyph -> return glyph
          Nothing ->
            let font = fromRight eFont
                fontScaleFactor = 1 / (fromIntegral $ F.unitsPerEm font)
                (advance, glyphVector) = F.getCharacterGlyphsAndMetrics font (chr . unCodePoint $ codepoint)
                header = font ^. fontHorizontalHeader
                (FI.FWord ascent ) = (fromJust header) ^. hheaAscent
                (FI.FWord descent) = (fromJust header) ^. hheaDescent
                contours :: [VU.Vector (Int16, Int16)]
                contours = if V.length glyphVector > 0
                           then (V.head glyphVector) ^. rawGlyphContour
                           else []
                height = ascent + descent
                vertices :: [[(Int16,Int16)]]
                vertices = map (map (flipY (fromIntegral $ height)) . VU.toList) contours
                shape :: [Outline DisplaySpace]
                shape =  map (Outline . pairPoints . map ((^* fontScaleFactor) . fmap fromIntegral . pairToPoint)) vertices
                glyph :: Glyph DisplaySpace
                glyph = Glyph { glyphVertices        = shape
                              , glyphAdvanceWidth    = Ortho $ realToFrac advance * fontScaleFactor
                              , glyphAscent          = Ortho $ realToFrac ascent  * fontScaleFactor
                              , glyphDescent         = Ortho $ realToFrac descent * fontScaleFactor
                              }
            in  do  gCMap .= M.insert codepoint glyph dict
                    return glyph

-- | Convert a string of characters to a list of glyphs in the GlyphMonad.
glyphString :: (MonadState GlyphCache m, Monad m) => String -> m [Glyph DisplaySpace]
glyphString = mapM (getGlyph . CodePoint . ord)
