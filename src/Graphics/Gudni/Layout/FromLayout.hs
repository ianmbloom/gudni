{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.FromLayout
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Intermediate data types used to traverse layouts
-----------------------------------------------------------------------------

module Graphics.Gudni.Layout.FromLayout
  ( FullProximityTree(..)
  , FullProximityCompoundTree(..)
  , SimpTree(..)
  , CollapseTree(..)
  , CollapseCompoundTree(..)
  , CollapseShapeTree(..)
  , FinalTree(..)
  , fromLayout
  , toFullProximityTree
  , traverseFullProximityCompoundTree
  , traverseFullProximityTree
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.ApplyProximity
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Layout

import Graphics.Gudni.Util.Util

import Control.Lens
import Control.Monad

type ProximityTree token style = TransTree (ProximityMeld style Overlap) (Maybe (SMask token NamedTexture (ProximityCompoundTree style)))

traverseSRep f (SMask token tex x) = SMask token tex <$> f x

joinItems :: TransTree meld (Maybe (TransTree meld (Maybe a))) -> TransTree meld (Maybe a)
joinItems = go
   where
   go tree =
       case tree of
           SMeld meld a b -> SMeld meld (go a) (go b)
           SLeaf (SBranch tag child) -> SLeaf . SBranch tag $ go child
           SLeaf (SItem item) ->
               case item of
                 Nothing -> SLeaf . SItem $ Nothing
                 Just x  -> x

traverseMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
traverseMaybe = traverse

traverseCompoundLayout f (CompoundLayout a) = f a


toProximityCompoundTree :: forall style m
                        .  ( IsStyle style
                           , Monad m
                           )
                        => CompoundLayout style
                        -> FontMonad style m (ProximityCompoundTree style)
toProximityCompoundTree (CompoundLayout compoundLayout) =
  let mapped :: FontMonad style m (TransTree (ProximityMeld style Compound) (Maybe (TransTree (ProximityMeld style Compound) (Maybe (WithBox (Shape (SpaceOf style)))))))
      mapped = mapMSItem (traverse (onLayoutRep)) compoundLayout
  in  joinItems <$> mapped

toProximityTree :: forall style m token
                .  ( IsStyle style
                   , Monad m
                   )
                => Layout style
                -> FontMonad style m (ProximityTree (TokenOf style) style)
toProximityTree (Layout tree) =
  mapMSItem (traverseMaybe (traverseSRep (toProximityCompoundTree))) tree

onLayoutRep :: ( IsStyle style
               , Monad m
               )
            => LayoutRep style
            -> FontMonad style m (ProximityCompoundTree style)
onLayoutRep rep =
    case rep of
         Glyph style codePoint  -> styleGlyph style codePoint
         LayoutShape shape ->
             let box = boxOf shape
             in  return . SLeaf . SItem . Just $ WithBox shape box

type FullProximityCompoundTree style = TransTree (ProximityMeld style Compound) (WithBox (Shape (SpaceOf style)))
type FullProximityTree token style = TransTree (ProximityMeld style Overlap) (SMask token NamedTexture (FullProximityCompoundTree style))

overSRepMaybe f (SMask token tex rep) =
  case f rep of
    Nothing -> Nothing
    Just x -> Just (SMask token tex x)

proximityTreeToFullProximityTree :: ProximityTree (TokenOf style) style -> Maybe (FullProximityTree (TokenOf style) style)
proximityTreeToFullProximityTree = fullSTree . mapSItem (join . fmapMaybe (overSRepMaybe fullSTree))

toFullProximityTree :: (IsStyle style, Monad m) => Layout style -> FontMonad style m (Maybe (FullProximityTree (TokenOf style) style))
toFullProximityTree layout =
  do proximityTree <- toProximityTree layout
     return $ proximityTreeToFullProximityTree proximityTree


fromWithItem (WithBox item _) = item
fromLayout :: forall style token m
           .  ( IsStyle style
              , Monad m
              )
           => Layout style
           -> FontMonad style m (Maybe (FinalTree (TokenOf style) (SpaceOf style)))
fromLayout layout =
  do full <- toFullProximityTree layout
     let collapsed :: Maybe (SimpTree Overlap (Tree Overlap (SMask (TokenOf style) NamedTexture (Tree Compound (Shape (SpaceOf style))))))
         collapsed = fmap (fromWithItem . collapseProximityTree) full
     return $ fmap (joinLeaves . collapseSimpTree) collapsed

data SimpTree_ meld item

instance HasMeld (SimpTree_ meld item) where
  type Meld (SimpTree_ meld item) = meld

instance (HasSpace item) => TreeType (SimpTree_ meld item) where
  type Leaf (SimpTree_ meld item) = SBranch (SimpTree_ meld item)

instance (HasSpace item) => TagTreeType (SimpTree_ meld item) where
  type Tag  (SimpTree_ meld item) = SimpleTransformer (SpaceOf item)
  type Item (SimpTree_ meld item) = item

type SimpTree meld item = STree (SimpTree_ meld item)

instance (HasSpace leaf, CanBox leaf) => CanBox (Tree meld leaf) where
    boxOf tree =
        case tree of
            SMeld meld a b -> minMaxBox (boxOf a) (boxOf b)
            SLeaf item -> boxOf item

instance (CanApplyProjection leaf) => CanApplyProjection (Tree meld leaf) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
        mapSLeaf (projectionWithStepsAccuracy debug max_steps m_accuracy path)

instance (HasSpace leaf, CanApplyTransformer leaf) => CanApplyTransformer (Tree meld leaf) where
    applyTransformer trans = mapSLeaf (applyTransformer trans)

instance (PointContainer a) => PointContainer (Tree meld a) where
    mapOverPoints f = mapSLeaf (mapOverPoints f)

instance CanApplySimpleTransformer leaf => CanApplySimpleTransformer (Tree meld leaf) where
    applySimpleTransformer trans = mapSLeaf (applySimpleTransformer trans)

type CollapseTree meld item = WithBox (SimpTree meld (Tree meld item))

type CollapseCompoundTree s = CollapseTree Compound (Shape s)
type CollapseShapeTree token s = CollapseTree Overlap (SMask token NamedTexture (Tree Compound (Shape s)))

instance (HasSpace leaf) => SimpleTransformable (STree (SimpTree_ meld leaf)) where
    translateBy delta tree = if delta == zeroPoint then tree else addBranch (Translate delta) tree
    stretchBy size tree    = if size == Point2 1 1 then tree else addBranch (Stretch size) tree
    simpleTransformWith t  = addBranch t

keep :: a -> b -> (b, b)
keep _ b = (b, b)

collapseTrans :: forall meld a
              .  ( HasSpace a
                 , CanApplyTransformer a
                 )
              => TransTree meld a
              -> Tree meld a
collapseTrans tree =
  go (Simple IdentityTransform) tree
  where
  go parentTrans tree =
     case tree of
         SMeld meld a b -> SMeld meld (go parentTrans a) (go parentTrans b)
         SLeaf (SBranch trans tree) -> go (CombineTransform parentTrans trans) tree
         SLeaf (SItem item) -> SLeaf (applyTransformer parentTrans item)

remeldCollapseTree :: ( HasSpace leaf
                      , IsStyle style
                      , SpaceOf style ~ SpaceOf leaf
                      )
                   => ProximityMeld style meld
                   -> CollapseTree meld leaf
                   -> CollapseTree meld leaf
                   -> CollapseTree meld leaf
remeldCollapseTree proximity a b =
  let (pa, pb) = applyProximity (proximity ^. proxStyle) (proximity ^. proxType) (a ^. withBox, b ^. withBox)
  in  WithBox (SMeld (proximity ^. proxMeld)
                     (translateBy pa $ a ^. withItem)
                     (translateBy pb $ b ^. withItem)
              )
              (minMaxBox (translateBox pa $ a ^. withBox)
                         (translateBox pb $ b ^. withBox)
              )

overWithItem f (WithBox item box) = WithBox (f item) box

instance PointContainer a => PointContainer (SMask token tex a) where
    mapOverPoints f = overSRep (mapOverPoints f)

instance CanApplyProjection a => CanApplyProjection (SMask token tex a) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
      overSRep (projectionWithStepsAccuracy debug max_steps m_accuracy path)

instance CanApplySimpleTransformer a => CanApplySimpleTransformer (SMask token NamedTexture a) where
    applySimpleTransformer trans = overSRep (applySimpleTransformer trans)

collapseProximityTree :: forall token style .
                         ( IsStyle style
                         --, SpaceOf (ShapeFunctor (Bezier (SpaceOf style)))~SpaceOf style
                         , Show token
                         )
                      => FullProximityTree token style
                      -> CollapseShapeTree token (SpaceOf style)
collapseProximityTree = go
    where
    go tree =
        case tree of
          SMeld meld a b -> remeldCollapseTree meld (go a) (go b)
          SLeaf (SBranch trans child) -> transformCollapseTree trans $ go child
          SLeaf (SItem (SMask token tex rep)) ->
              let collapsed = collapseProximityCompoundTree $ rep
                  f :: SimpTree Compound (Tree Compound (Shape (SpaceOf style)))
                    -> SimpTree Overlap  (Tree Overlap  (SMask token NamedTexture (Tree Compound (Shape (SpaceOf style)))))
                  f = SLeaf . SItem . SLeaf . SMask token tex . joinLeaves . collapseSimpTree
              in  overWithItem f collapsed

collapseProximityCompoundTree :: ( IsStyle style
                                 --, SpaceOf (ShapeFunctor (Bezier (SpaceOf style)))~SpaceOf style
                                 )
                              => FullProximityCompoundTree style
                              -> CollapseCompoundTree (SpaceOf style)
collapseProximityCompoundTree = go
    where
    go tree =
        case tree of
            SMeld meld a b -> remeldCollapseTree meld (go a) (go b)
            SLeaf (SBranch trans child) -> transformCollapseTree trans $ go child
            SLeaf (SItem (WithBox item box)) -> WithBox (SLeaf . SItem . SLeaf $ item) box

collapseSimpTree :: (CanApplySimpleTransformer leaf) => SimpTree meld leaf -> Tree meld leaf
collapseSimpTree tree =
  go IdentityTransform tree
  where
  go trans tree =
      case tree of
          SMeld meld a b -> SMeld meld (go trans a) (go trans b)
          SLeaf (SBranch tag child) -> go (CombineSimple tag trans) child
          SLeaf (SItem item) -> SLeaf $ applySimpleTransformer trans item

transformCollapseTree :: ( HasSpace item
                         , CanBox item
                         , CanApplyProjection item
                         , PointContainer item
                         , CanApplySimpleTransformer item
                         )
                      => Transformer (SpaceOf item)
                      -> CollapseTree meld item
                      -> CollapseTree meld item
transformCollapseTree trans (WithBox tree box) =
  case trans of
    Simple simp -> WithBox (addBranch simp tree) (applySimpleTransformer simp box)
    _ -> let collapsed = collapseSimpTree tree
             transformed = case trans of
                               Rotate angle -> applyRotation angle collapsed
                               Project path -> let bSpace = makeBezierSpace arcLength path
                                               in projectDefault False bSpace collapsed
             newTree = mapSLeaf SItem transformed
             newBox  = boxOf transformed
         in WithBox newTree newBox

-- For Fabric
traverseProximityCompound :: ProximityMeld style Compound
                          -> ProximityMeld style Compound
                          -> (ProximityMeld style Compound, ProximityMeld style Compound)
traverseProximityCompound (ProximityMeld proxA styleA CompoundAdd)
                          (ProximityMeld proxB styleB current) = ( ProximityMeld proxA styleA current
                                                                 , ProximityMeld proxB styleB current)
traverseProximityCompound (ProximityMeld proxA styleA CompoundSubtract)
                          (ProximityMeld proxB styleB current) = ( ProximityMeld proxA styleA (invertCompound current)
                                                                 , ProximityMeld proxB styleB current)

traverseFullProximityCompoundTree :: forall  m i value style
                                  .  ( Monad m
                                     , SpaceOf (Item i) ~ SpaceOf style
                                     , HasDefault style
                                     , Meld i ~ ProximityMeld style Compound
                                     , Tag  i ~ Transformer (SpaceOf (Item i))
                                     , Leaf i ~ SBranch i
                                     )
                                  => (Tag  i -> value -> value )
                                  -> (Meld i -> value -> value -> value)
                                     -- onItem
                                  -> (Meld i -> Tag i -> Item i -> m value)
                                  -> Tag i
                                  -> STree i
                                  -> m value
traverseFullProximityCompoundTree buildTrans meldValues onItem parentTrans tree =
  let trTree   = traverseSTree traverseProximityCompound meldValues
      trBranch = traverseSBranch buildTrans CombineTransform onItem
  in  trTree trBranch defaultValue parentTrans tree

-- | Traverse an overlap shape tree
traverseFullProximityTree :: forall  m i value style
                          .  ( Monad m
                             , SpaceOf (Item i) ~ SpaceOf style
                             , HasDefault style
                             , Meld i ~ ProximityMeld style Overlap
                             , Tag  i ~ Transformer (SpaceOf (Item i))
                             , Leaf i ~ SBranch i
                             )
                          => (Tag  i -> value -> value )
                          -> (Meld i -> value -> value -> value)
                          -> (Meld i -> Tag i -> Item i -> m value)
                          -> STree i
                          -> m value
traverseFullProximityTree buildTrans meldValues onItem tree =
  let trTree   = traverseSTree keepMeld meldValues
      trBranch = traverseSBranch buildTrans CombineTransform onItem
  in  trTree trBranch (defaultValue :: Meld i) (Simple IdentityTransform) tree
