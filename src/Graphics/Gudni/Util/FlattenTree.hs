-- {-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.FlattenTree
  ( CanFlatten(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree

import qualified Data.Sequence as S
import Control.Monad.State
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Util
import Control.Monad.Identity

import Control.Applicative

class CanFlatten t where
    type ToFlatten t :: *
    flatten :: t -> S.Seq (ToFlatten t)

instance CanFlatten (Leaf i) => CanFlatten (STree i) where
    type ToFlatten (STree i) = ToFlatten (Leaf i)
    flatten tree =
        case tree of
            SMeld meld a b ->
                let a' = flatten a
                    b' = flatten b
                in  a' <|> b'
            SLeaf x -> flatten x

instance CanFlatten t => CanFlatten (SRep token sub t) where
    type ToFlatten (SRep token sub t) = ToFlatten t
    flatten (SRep token sub t) = flatten t

instance CanFlatten (Shape s) where
    type ToFlatten (Shape s) = Shape s
    flatten shape = pure shape
