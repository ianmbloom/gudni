{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Raster.Dag.FromLayout
  ( PicturePass(..)
  , layoutToFabric
  , sceneToFabric
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.ShapeTree.STree
import Graphics.Gudni.ShapeTree.Traverse

import Graphics.Gudni.Raster.Dag.Fabric
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Layout.Layout

import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.FromLayout
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Token

import Control.Monad.Identity
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map as M

data LayoutStart s

instance HasSpace style => HasSpace (LayoutStart style) where
    type SpaceOf (LayoutStart style) = SpaceOf style

instance HasSpace style => FabricType (LayoutStart style) where
    type FChild    (LayoutStart style) = Fabric (LayoutStart style)
    type FTex      (LayoutStart style) = NamedTexture
    type FGeometry (LayoutStart style) = WithBox (Shape (SpaceOf style))
    type FQuery    (LayoutStart style) = Color
    type FCombiner (LayoutStart style) = ProximityMeld style FCombineType

buildFacets = undefined

buildTransform :: ( FabricType i
                  , FChild i ~ Fabric i
                  )
               => Transformer (SpaceOf i)
               -> Fabric i
               -> Fabric i
buildTransform = go
  where
  go transform =
      case transform of
          Rotate a -> FTransform $ FAffineRay (affineRotate a)
          Project curve -> FTransform (FFacet (buildFacets curve))
          CombineTransform a b -> go a . go b
          Simple simple -> buildSimpleTransform simple

buildSimpleTransform :: ( FabricType i
                        , FChild i ~ Fabric i
                        )
                     => SimpleTransformer (SpaceOf i)
                     -> Fabric i
                     -> Fabric i
buildSimpleTransform = go
    where
    go simple =
     case simple of
         IdentityTransform -> FTransform $ FAffineRay affineIdentity
         Translate p -> FTransform $ FAffineRay (affineTranslate p)
         Stretch p -> FTransform $ FAffineRay (affineStretch p)
         CombineSimple a b -> go a . go b

-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
rShape :: ProximityMeld style Compound
       -> Transformer (SpaceOf style)
       -> WithBox (Shape (SpaceOf style)) -- TODO: box not needed
       -> Identity (Fabric (LayoutStart style))
rShape _ transform shape = return $ FLeaf $ FGeometry shape

noProx :: HasDefault style => a -> ProximityMeld style a
noProx = ProximityMeld noProximity defaultValue

compoundRCombiner :: HasDefault style
                  => ProximityMeld style Compound
                  -> Fabric (LayoutStart style)
                  -> Fabric (LayoutStart style)
                  -> Fabric (LayoutStart style)
compoundRCombiner meld above below =
    case meld ^. proxMeld of
       CompoundAdd      -> FCombine (set proxMeld FFloatOr meld) above below
       CompoundSubtract -> FCombine (set proxMeld FMask    meld) (FTransform (FFilter FInvert) above) below

-- | For each shape in the shapeTree serialize the substance metadata and serialize the compound subtree.
rSubstance :: forall m item token style
           .  ( IsStyle style
              )
           => ProximityMeld style Overlap
           -> Transformer (SpaceOf style)
           -> SMask token (FTex (LayoutStart style)) (FullProximityCompoundTree style)
           -> Identity (Fabric (LayoutStart style))
rSubstance meld transform (SMask mToken substance subTree) =
    do let rSubstance = case substance of
                            Solid color -> FConst color
                            Texture tex -> FTexture tex
                            Linear grad -> FLinear
                            Radial grad -> FQuadrance
           parentMask = runIdentity (traverseFullProximityCompoundTree compoundRCombiner rShape defaultValue subTree)
       return $ FCombine (noProx FMask) parentMask (FLeaf rSubstance)


compressAffine :: (FabricType i, FChild i ~ Fabric i) => Fabric i -> Fabric i
compressAffine = go
  where
  go tree =
      case tree of
        FCombine comb a b -> FCombine comb (go a) (go b)
        FTransform (FAffineRay a) (FTransform (FAffineRay b) child) -> go (FTransform (FAffineRay (composeAffine a b)) child)
        FTransform t child -> FTransform t (go child)
        FLeaf l -> FLeaf l

data PicturePass style

instance HasSpace style => HasSpace (PicturePass style) where
    type SpaceOf (PicturePass style) = SpaceOf style

instance HasSpace style => FabricType (PicturePass style) where
    type FChild    (PicturePass style) = Fabric (PicturePass style)
    type FTex      (PicturePass style) = PictureMemoryReference
    type FGeometry (PicturePass style) = WithBox (Shape (SpaceOf style))
    type FQuery    (PicturePass style) = Color
    type FCombiner (PicturePass style) = ProximityMeld style FCombineType

sharedOnly :: NamedTexture -> PictureName
sharedOnly tex =
    case tex of
        SharedTexture name -> name
        NewTexture {} -> error "new textures not supported"

assignPictUsage :: PictureMemoryMap -> FSubstance (LayoutStart style) -> FSubstance (PicturePass style)
assignPictUsage pictureMemoryMap substance =
    case substance of
        FGeometry shape -> FGeometry shape
        FConst    q     -> FConst q
        FTexture  name  -> FTexture $ (M.!) pictureMemoryMap (sharedOnly name)
        FLinear         -> FLinear
        FQuadrance      -> FQuadrance

assignPictures :: PictureMemoryMap -> Fabric (LayoutStart style) -> Fabric (PicturePass style)
assignPictures pictureMemoryMap = go
    where
    go tree =
        case tree of
            FCombine   ty a b      -> FCombine ty (go a) (go b)
            FTransform trans child -> FTransform trans $ go child
            FLeaf      leaf        -> FLeaf $ assignPictUsage pictureMemoryMap leaf

overlapCombiner (ProximityMeld prox style Overlap) above below = FCombine (ProximityMeld prox style FComposite) above below

layoutToFabric :: forall m style
               .  ( Monad m
                  , IsStyle style
                  )
               => PictureMemoryMap
               -> Color
               -> Layout style
               -> FontMonad style m (Fabric (PicturePass style))
layoutToFabric pictureMemoryMap backgroundColor layout =
  do mFull <- toFullProximityTree layout
     let background = FLeaf $ FConst backgroundColor
     case mFull of
       Nothing -> return background
       Just full -> return .
                    flip (FCombine (noProx FComposite)) background .
                    assignPictures pictureMemoryMap .
                    compressAffine $
                     (runIdentity (traverseFullProximityTree overlapCombiner rSubstance full))

sceneToFabric :: forall m style
              .  ( Monad m
                 , IsStyle style
                 )
              => PictureMemoryMap
              -> Scene (Layout style)
              -> FontMonad style m (Fabric (PicturePass style))
sceneToFabric pictureMemoryMap scene =
    layoutToFabric pictureMemoryMap (scene ^. sceneBackgroundColor) (scene ^. sceneShapeTree)
