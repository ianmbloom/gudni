{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.StorableInstances
    (
    )
where
------------------ Storable ------------------------

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.StorableM

import Foreign.C.Types
import Foreign.Ptr
import Linear.V2
import Linear.V3

instance Storable SubSpace where
    sizeOf    _         = sizeOf    (undefined :: SubSpace_)
    alignment _         = alignment (undefined :: SubSpace_)
    peek ptr            = SubSpace . (realToFrac :: CFloat -> SubSpace_) <$> peek (castPtr ptr)
    poke ptr (SubSpace x) = poke (castPtr ptr) $ (realToFrac :: SubSpace_ -> CFloat) x

instance Storable PixelSpace where
    sizeOf   _          = sizeOf    (undefined :: PixelSpace_)
    alignment _         = alignment (undefined :: PixelSpace_)
    peek ptr            = PSpace . fromIntegral <$> (peek (castPtr ptr) :: IO CInt)
    poke ptr (PSpace x) = poke (castPtr ptr) (fromIntegral x :: CInt)

instance (Storable s) => StorableM (Box s) where
  sizeOfM _ = do sizeOfM (undefined :: Point2 s)
                 sizeOfM (undefined :: Point2 s)
  alignmentM _ = do alignmentM (undefined :: Point2 s)
                    alignmentM (undefined :: Point2 s)
  peekM =
    do topLeft     <- peekM
       bottomRight <- peekM
       return (Box topLeft bottomRight)
  pokeM (Box topLeft bottomRight) =
    do pokeM topLeft
       pokeM bottomRight

instance (Storable s) => Storable (Box s) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance Storable s => StorableM (Bezier s) where
    sizeOfM _ = do sizeOfM (undefined :: V3 (Point2 s))
    alignmentM _ = do alignmentM (undefined :: V3 (Point2 s))
    peekM = Bezier <$> peekM
    pokeM (Bezier v3) = pokeM v3

instance Storable s => Storable (Bezier s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance Storable s => StorableM (Facet s) where
    sizeOfM _ = do sizeOfM (undefined :: V3 (V2 (Point2 s )))
                   sizeOfM (undefined :: V3 (Point2 s))
    alignmentM _ = do alignmentM (undefined :: V3 (V2 (Point2 s)))
                      alignmentM (undefined :: V3 (Point2 s) )
    peekM = do scene   <- peekM
               texture <- peekM
               return $ Facet scene texture
    pokeM (Facet scene texture) = do pokeM scene
                                     pokeM texture

instance Storable s => Storable (Facet s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance Storable s => StorableM (Affine s) where
    sizeOfM _ = do sizeOfM (undefined :: V3 (V3 s ))
    alignmentM _ = do alignmentM (undefined :: V3 (V3 s ))
    peekM = do matrix  <- peekM
               return $ Affine matrix
    pokeM (Affine matrix) = do pokeM matrix

instance Storable s => Storable (Affine s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV

instance Storable s => StorableM (Affine s, Affine s) where
    sizeOfM _ = do sizeOfM (undefined :: Affine s)
                   sizeOfM (undefined :: Affine s)
    alignmentM _ = do alignmentM (undefined :: Affine s)
                      alignmentM (undefined :: Affine s)
    peekM = do forward <- peekM
               back    <- peekM
               return $ (forward, back)
    pokeM (forward, back) = do pokeM forward
                               pokeM back

instance Storable s => Storable (Affine s, Affine s) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
