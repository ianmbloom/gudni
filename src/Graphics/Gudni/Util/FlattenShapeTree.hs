-- {-# LANGUAGE ScopedTypeVariables  #-}
-- {-# LANGUAGE FlexibleContexts     #-}
-- {-# LANGUAGE TypeFamilies         #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.FlattenShapeTree
  ( flattenShapeTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree

import qualified Data.Sequence as S
import Control.Monad.State
import Graphics.Gudni.Util.Loop

const3 :: a -> b -> c -> d -> a
const3 a b c d = a

flattenShapeTree :: (Space s, Show token) => ShapeTree token textureLabel s
                 -> S.Seq (Shape s)
flattenShapeTree tree =
    case fullSTree tree of
        Nothing -> S.empty
        Just full -> execState (traverseShapeTree flattenSubstance (const3 ()) full) S.empty

flattenCompoundTree :: (Space s)
                    => Transformer s
                    -> CompoundTree s
                    -> State (S.Seq (Shape s)) ()
flattenCompoundTree transformer =
    traverseCompoundTree defaultValue transformer flattenShape (const3 ())

flattenSubstance :: (Space s) => Overlap -> Transformer s -> SRep token sub (CompoundTree s) -> State (S.Seq (Shape s))  ()
flattenSubstance Overlap transformer (SRep token substance subTree) =
    flattenCompoundTree transformer subTree

flattenShape :: ( Space s
                , Loop f
                )
             => Compound
             -> Transformer s
             -> Shape_ f s
             -> State (S.Seq (Shape_ f s)) ()
flattenShape combineType transformer shape =
    let shape' = applyTransformer transformer shape
    in  modify (flip (S.|>) shape')
