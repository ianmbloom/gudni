{-# LANGUAGE LambdaCase #-}

module Graphics.Gudni.Raster.Merkle
  (
  )
where

import Graphics.Gudni.Raster.Types

data MerkleTree a = MTree (Digest a) (MerkleTree a) (MerkleTree a) | MLeaf (Digest a) Block PrimId

constructMerkle :: STree token rep -> MerkleTree a
constructMerkle = \case
    ShapeGroup substance token compoundTree ->
        let compoundDigest = MLeaf (
