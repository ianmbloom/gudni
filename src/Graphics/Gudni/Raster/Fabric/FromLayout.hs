{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Raster.Fabric.FromLayout
  ( LayoutStart(..)
  , PicturePass(..)
  , prepFabric
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Token

import Control.Monad.Identity
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map as M

-- | LayoutStart

data LayoutStart var style

instance HasSpace style => HasSpace (LayoutStart var style) where
    type SpaceOf (LayoutStart var style) = SpaceOf style

instance IsStyle style => HasStyle (LayoutStart var style) where
    type StyleOf (LayoutStart var style) = style

instance (HasSpace style) => FabricType (LayoutStart var style) where
    type FChildType  (LayoutStart var style) = Fabric (LayoutStart var style)
    type FBinaryType (LayoutStart var style) = ProximityMeld style FCombineType
    type FPostType   (LayoutStart var style) = FFilter
    type FPreType    (LayoutStart var style) = FTransformer (SpaceOf style)
    type FLeafType   (LayoutStart var style) = GlyphLeaf (style, CodePoint) (LayoutStart var style)
    type FVarName    (LayoutStart var style) = var

instance HasSpace style => SubstanceType (LayoutStart var style) where
    type FTex        (LayoutStart var style) = NamedTexture
    type FQuery      (LayoutStart var style) = Color (SpaceOf style)

-- | PicturePass

data PicturePass var style

instance HasSpace style => HasSpace (PicturePass var style) where
    type SpaceOf (PicturePass var style) = SpaceOf style

instance IsStyle style => HasStyle (PicturePass var style) where
    type StyleOf (PicturePass var style) = style

instance (HasSpace style) => FabricType (PicturePass var style) where
    type FChildType  (PicturePass var style) = Fabric (PicturePass var style)
    type FBinaryType (PicturePass var style) = ProximityMeld style FCombineType
    type FPostType   (PicturePass var style) = FFilter
    type FPreType    (PicturePass var style) = FTransformer (SpaceOf style)
    type FLeafType   (PicturePass var style) = GlyphLeaf (style, CodePoint) (PicturePass var style)
    type FVarName    (PicturePass var style) = var

instance HasSpace style => SubstanceType (PicturePass var style) where
    type FTex        (PicturePass var style) = PictureMemoryReference
    type FQuery      (PicturePass var style) = Color (SpaceOf style)

compressAffine :: ( FabricType i
                  , FChildType i ~ Fabric i
                  , FPreType i ~ FTransformer (SpaceOf i)
                  ) => Fabric i
                    -> Fabric i
compressAffine = go
  where
  go tree =
      case tree of
        FBinary comb a b -> FBinary comb (go a) (go b)
        FUnaryPre (FAffine aF aB) (FUnaryPre (FAffine bF bB) child) ->
            go (FUnaryPre (FAffine (composeAffine aF bF) (composeAffine aB bB)) child)
        FUnaryPre  t child -> FUnaryPre  t (go child)
        FUnaryPost f child -> FUnaryPost f (go child)
        FLeaf l -> FLeaf l

removeAffineIdentity :: ( FabricType i
                        , FChildType i ~ Fabric i
                        , FPreType i ~ FTransformer (SpaceOf i)
                        )
                     => Fabric i
                     -> Fabric i
removeAffineIdentity = go
  where
  go tree =
      case tree of
        FBinary comb a b -> FBinary comb (go a) (go b)
        FUnaryPre (FAffine aF aB) child ->
            if aF == affineIdentity
            then go child
            else FUnaryPre (FAffine aF aB) (go child)
        FUnaryPre  t child -> FUnaryPre  t child
        FUnaryPost t child -> FUnaryPost t child
        FLeaf l -> FLeaf l

sharedOnly :: NamedTexture -> PictureName
sharedOnly tex =
    case tex of
        SharedTexture name -> name
        NewTexture {} -> error "new textures not supported"

assignPictUsage :: PictureMemoryMap -> FSubstance (LayoutStart var style) -> FSubstance (PicturePass var style)
assignPictUsage pictureMemoryMap substance =
    case substance of
        FConst    q     -> FConst q
        FTexture  name  -> FTexture $ (M.!) pictureMemoryMap (sharedOnly name)
        FLinear         -> FLinear
        FQuadrance      -> FQuadrance

assignPictures :: PictureMemoryMap -> Fabric (LayoutStart var style) -> Fabric (PicturePass var style)
assignPictures pictureMemoryMap = go
    where
    go tree =
        case tree of
            FBinary    ty   a b   -> FBinary ty (go a) (go b)
            FUnaryPre  pre  child -> FUnaryPre  pre  $ go child
            FUnaryPost post child -> FUnaryPost post $ go child
            FLeaf      leaf       -> FLeaf $
                case leaf of
                    GlyphLeaf glyph -> GlyphLeaf glyph
                    ShapeLeaf shapeLeaf -> ShapeLeaf $
                        case shapeLeaf of
                            FShape     shape     -> FShape shape
                            FSubstance substance -> FSubstance $ assignPictUsage pictureMemoryMap substance

prepFabric :: forall style var
           .  ( IsStyle style )
           => PictureMemoryMap
           -> Fabric (LayoutStart var style)
           -> Fabric (PicturePass var style)
prepFabric pictureMemoryMap =
    removeAffineIdentity . -- optional
    compressAffine . -- optional
    assignPictures pictureMemoryMap
