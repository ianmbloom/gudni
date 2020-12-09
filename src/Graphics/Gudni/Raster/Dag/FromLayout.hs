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

import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Dag.Fabric.Filter.Type
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
    type FChildType  (LayoutStart style) = Fabric (LayoutStart style)
    type FBinaryType (LayoutStart style) = ProximityMeld style FCombineType
    type FPostType   (LayoutStart style) = FFilter
    type FPreType    (LayoutStart style) = FTransformer (SpaceOf style)
    type FLeafType   (LayoutStart style) = FLeaf (LayoutStart style)

instance HasSpace style => SubstanceType (LayoutStart style) where
    type FTex      (LayoutStart style) = NamedTexture
    type FQuery    (LayoutStart style) = Color (SpaceOf style)

buildFacets = undefined

buildTransform :: ( FabricType i
                  , FChildType i ~ Fabric i
                  , FPreType i ~ FTransformer (SpaceOf i)
                  )
               => Transformer (SpaceOf i)
               -> Fabric i
               -> Fabric i
buildTransform = go
  where
  go transform =
      case transform of
          Rotate a      -> FUnaryPre $ FAffine (affineRotate (negateAngle a)) (affineRotate a)
          Project curve -> FUnaryPre . FFacet . buildFacets $ curve
          CombineTransform a b -> go a . go b
          Simple simple -> buildSimpleTransform simple

safeInvert :: Space s => s -> s
safeInvert 0 = maxBound
safeInvert x = 1 / x

buildSimpleTransform :: ( FabricType i
                        , FChildType i ~ Fabric i
                        , FPreType i ~ FTransformer (SpaceOf i)
                        )
                     => SimpleTransformer (SpaceOf i)
                     -> Fabric i
                     -> Fabric i
buildSimpleTransform = go
    where
    go simple =
     case simple of
         IdentityTransform -> FUnaryPre $ FAffine  affineIdentity                        affineIdentity
         Translate p       -> FUnaryPre $ FAffine (affineTranslate (negate          p)) (affineTranslate p)
         Stretch p         -> FUnaryPre $ FAffine (affineStretch   (fmap safeInvert p)) (affineStretch   p)
         CombineSimple a b -> go a . go b

-- | On each shape in the shape tree run add the appropriate data to the appropriate buffers and the TileTree.
rShape :: forall style
       .  ( IsStyle style
          )
       => ProximityMeld style Compound
       -> Transformer (SpaceOf style)
       -> WithBox (Shape (SpaceOf style)) -- TODO: box not needed
       -> Identity (Fabric (LayoutStart style))
rShape _ transform shape = return . FLeaf . FShape $ shape

noProx :: HasDefault style => a -> ProximityMeld style a
noProx = ProximityMeld noProximity defaultValue

compoundRCombiner :: HasDefault style
                  => ProximityMeld style Compound
                  -> Fabric (LayoutStart style)
                  -> Fabric (LayoutStart style)
                  -> Fabric (LayoutStart style)
compoundRCombiner meld above below =
    case meld ^. proxMeld of
       CompoundAdd      -> FBinary (set proxMeld FFloatOr meld) above below
       CompoundSubtract -> FBinary (set proxMeld FMask    meld) (FUnaryPost FInvert above) below

-- | For each shape in the shapeTree serialize the substance metadata and serialize the compound subtree.
rSubstance :: forall m item token style
           .  ( IsStyle style
              )
           => ProximityMeld style Overlap
           -> Transformer (SpaceOf style)
           -> SMask token (FTex (LayoutStart style)) (FullProximityCompoundTree style)
           -> Identity (Fabric (LayoutStart style))
rSubstance meld _ (SMask mToken substance subTree) =
    do let rSubstance = case substance of
                            Solid color -> FConst color
                            Texture tex -> FTexture tex
                            Linear grad -> FLinear
                            Radial grad -> FQuadrance
           parentMask = runIdentity $
                        traverseFullProximityCompoundTree buildTransform
                                                          compoundRCombiner
                                                          rShape
                                                          defaultValue
                                                          subTree
       return $ FBinary (noProx FMask) parentMask (FLeaf . FSubstance $ rSubstance)


compressAffine :: ( FabricType i
                  , FChildType i ~ Fabric i
                  , FPreType i ~ FTransformer (SpaceOf i)
                  ) => Fabric i -> Fabric i
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

removeAffineIdentity :: ( IsStyle style )
                     => Fabric (LayoutStart style)
                     -> Fabric (LayoutStart style)
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

data PicturePass style

instance HasSpace style => HasSpace (PicturePass style) where
    type SpaceOf (PicturePass style) = SpaceOf style

instance HasSpace style => FabricType (PicturePass style) where
    type FChildType  (PicturePass style) = Fabric (PicturePass style)
    type FBinaryType (PicturePass style) = ProximityMeld style FCombineType
    type FPostType   (PicturePass style) = FFilter
    type FPreType    (PicturePass style) = FTransformer (SpaceOf style)
    type FLeafType   (PicturePass style) = FLeaf (PicturePass style)

instance HasSpace style => SubstanceType (PicturePass style) where
    type FTex      (PicturePass style) = PictureMemoryReference
    type FQuery    (PicturePass style) = Color (SpaceOf style)

sharedOnly :: NamedTexture -> PictureName
sharedOnly tex =
    case tex of
        SharedTexture name -> name
        NewTexture {} -> error "new textures not supported"

assignPictUsage :: PictureMemoryMap -> FSubstance (LayoutStart style) -> FSubstance (PicturePass style)
assignPictUsage pictureMemoryMap substance =
    case substance of
        FConst    q     -> FConst q
        FTexture  name  -> FTexture $ (M.!) pictureMemoryMap (sharedOnly name)
        FLinear         -> FLinear
        FQuadrance      -> FQuadrance

assignPictures :: PictureMemoryMap -> Fabric (LayoutStart style) -> Fabric (PicturePass style)
assignPictures pictureMemoryMap = go
    where
    go tree =
        case tree of
            FBinary ty a b        -> FBinary ty (go a) (go b)
            FUnaryPre  pre  child -> FUnaryPre  pre  $ go child
            FUnaryPost post child -> FUnaryPost post $ go child
            FLeaf      leaf        -> FLeaf $
                 case leaf of
                     FShape shape -> FShape shape
                     FSubstance substance -> FSubstance $ assignPictUsage pictureMemoryMap substance

overlapCombiner (ProximityMeld prox style Overlap) above below = FBinary (ProximityMeld prox style FComposite) above below

layoutToFabric :: forall m style
               .  ( Monad m
                  , IsStyle style
                  )
               => PictureMemoryMap
               -> Color (SpaceOf style)
               -> Layout style
               -> FontMonad style m (Fabric (PicturePass style))
layoutToFabric pictureMemoryMap backgroundColor layout =
  do mFull <- toFullProximityTree layout
     let background = FLeaf $ FSubstance $ FConst backgroundColor
     case mFull of
       Nothing -> return background
       Just full -> return .
                    flip (FBinary (noProx FComposite)) background .
                    assignPictures pictureMemoryMap .
                    -- removeAffineIdentity .
                    compressAffine .
                    runIdentity $
                    traverseFullProximityTree buildTransform
                                              overlapCombiner
                                              rSubstance
                                              full

sceneToFabric :: forall m style
              .  ( Monad m
                 , IsStyle style
                 )
              => PictureMemoryMap
              -> Scene (Layout style)
              -> FontMonad style m (Fabric (PicturePass style))
sceneToFabric pictureMemoryMap scene =
    layoutToFabric pictureMemoryMap (scene ^. sceneBackgroundColor) (scene ^. sceneShapeTree)
