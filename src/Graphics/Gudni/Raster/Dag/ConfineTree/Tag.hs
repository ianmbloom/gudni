{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Tag
  ( ConfineTag(..)
  , ConfineTagId(..)
  , confineTagPrimTagId
  , confineTagCut
  , confineTagOverhang
  , confineTagLessCut
  , confineTagMoreCut
  , nullConfineTagId
  , DecoTag(..)
  , DecoTagId(..)
  , decoTagCut
  , decoTagCrossings
  , decoTagLessCut
  , decoTagMoreCut
  , nullDecoTagId
  , Root(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Raster.Dag.TagTypes

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice

import Graphics.Gudni.Util.StorableM

import Control.Lens
import Foreign.Storable
import GHC.Ptr

newtype ConfineTagId s = ConfineTagId {unConfineTagId :: Reference (ConfineTag s) } deriving (Eq, Ord, Generic, Num)

data ConfineTag s
    = ConfineTag
     { _confineTagPrimTagId :: PrimTagId
     , _confineTagCut       :: s
     , _confineTagOverhang  :: s
     , _confineTagLessCut   :: ConfineTagId s
     , _confineTagMoreCut   :: ConfineTagId s
     }
makeLenses ''ConfineTag

nullConfineTagId = ConfineTagId nullReference

newtype DecoTagId s = DecoTagId {unDecoTagId :: Reference (DecoTag s)} deriving (Eq, Ord, Generic, Num)

data DecoTag s
    = DecoTag
    { _decoTagCut       :: s
    , _decoTagCrossings :: Slice ShapeId
    , _decoTagLessCut   :: DecoTagId s
    , _decoTagMoreCut   :: DecoTagId s
    }
makeLenses ''DecoTag

nullDecoTagId = DecoTagId nullReference

type Root s = (ConfineTagId s, DecoTagId s)

instance Show (ConfineTagId s) where
  show = show . unConfineTagId
instance Show (DecoTagId s) where
  show = show . unDecoTagId

instance Out (ConfineTagId s) where
    doc treeId = text "Conf" <+> (doc . unConfineTagId $ treeId)
    docPrec _  = doc
instance Out (DecoTagId s) where
    doc treeId = text "Deco" <+> (doc . unDecoTagId $ treeId)
    docPrec _  = doc

instance Storable s => StorableM (ConfineTag s) where
    sizeOfM _ = do sizeOfM (undefined :: PrimTagId     )
                   sizeOfM (undefined :: s             )
                   sizeOfM (undefined :: s             )
                   sizeOfM (undefined :: ConfineTagId s)
                   sizeOfM (undefined :: ConfineTagId s)
    alignmentM _ = do alignmentM (undefined :: PrimTagId     )
                      alignmentM (undefined :: s             )
                      alignmentM (undefined :: s             )
                      alignmentM (undefined :: ConfineTagId s)
                      alignmentM (undefined :: ConfineTagId s)
    peekM = do tagPrimTagId <- peekM
               tagCut       <- peekM
               tagOverhang  <- peekM
               tagLessCut   <- peekM
               tagMoreCut   <- peekM
               return $ ConfineTag
                        { _confineTagPrimTagId = tagPrimTagId
                        , _confineTagCut       = tagCut
                        , _confineTagOverhang  = tagOverhang
                        , _confineTagLessCut   = tagLessCut
                        , _confineTagMoreCut   = tagMoreCut
                        }
    pokeM tag = do pokeM (tag ^. confineTagPrimTagId )
                   pokeM (tag ^. confineTagCut       )
                   pokeM (tag ^. confineTagOverhang  )
                   pokeM (tag ^. confineTagLessCut   )
                   pokeM (tag ^. confineTagMoreCut   )

instance Storable s => Storable (ConfineTag s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance forall s . Storable s => StorableM (DecoTag s) where
    sizeOfM _ = do sizeOfM (undefined :: s             )
                   sizeOfM (undefined :: Slice ShapeId )
                   sizeOfM (undefined :: DecoTagId s   )
                   sizeOfM (undefined :: DecoTagId s   )
    alignmentM _ = do alignmentM (undefined :: s             )
                      alignmentM (undefined :: Slice ShapeId )
                      alignmentM (undefined :: DecoTagId s   )
                      alignmentM (undefined :: DecoTagId s   )
    peekM = do tagCut       <- peekM
               tagCrossings <- peekM
               tagLessCut   <- peekM
               tagMoreCut   <- peekM
               return $ DecoTag
                        { _decoTagCut       = tagCut
                        , _decoTagCrossings = tagCrossings
                        , _decoTagLessCut   = tagLessCut
                        , _decoTagMoreCut   = tagMoreCut
                        }
    pokeM tag = do pokeM (tag ^. decoTagCut       )
                   pokeM (tag ^. decoTagCrossings )
                   pokeM (tag ^. decoTagLessCut   )
                   pokeM (tag ^. decoTagMoreCut   )

instance Storable s => Storable (DecoTag s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance Storable (ConfineTagId s) where
  sizeOf    (ConfineTagId i) = sizeOf    (undefined :: Reference (ConfineTag s))
  alignment (ConfineTagId i) = alignment (undefined :: Reference (ConfineTag s))
  peek ptr = ConfineTagId <$>  peek (castPtr ptr)
  poke ptr  (ConfineTagId i) = poke (castPtr ptr) i

instance Storable (DecoTagId s) where
  sizeOf    (DecoTagId i) = sizeOf    (undefined :: Reference (DecoTag s))
  alignment (DecoTagId i) = alignment (undefined :: Reference (DecoTag s))
  peek ptr = DecoTagId <$>  peek (castPtr ptr)
  poke ptr  (DecoTagId i) = poke (castPtr ptr) i

instance Storable s => StorableM (Root s) where
    sizeOfM _ = do sizeOfM (undefined :: Reference (ConfineTag s))
                   sizeOfM (undefined :: Reference (DecoTag s)   )
    alignmentM _ = do alignmentM (undefined :: Reference (ConfineTag s))
                      alignmentM (undefined :: Reference (DecoTag s)   )
    peekM = do confine <- peekM
               deco    <- peekM
               return $ (confine, deco)
    pokeM (confine, deco) = do pokeM confine
                               pokeM deco

instance Storable s => Storable (Root s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
