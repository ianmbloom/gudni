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


instance StorableM (RadialGradient SubSpace) where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 SubSpace)
       sizeOfM (undefined :: SubSpace       )
       sizeOfM (undefined :: SubSpace       )
       sizeOfM (undefined :: Color          )
       sizeOfM (undefined :: Color          )
  alignmentM _ =
    do alignmentM (undefined :: Point2 SubSpace)
       alignmentM (undefined :: SubSpace       )
       alignmentM (undefined :: SubSpace       )
       alignmentM (undefined :: Color          )
       alignmentM (undefined :: Color          )
  peekM = do center       <- peekM
             innerRadius  <- peekM
             outerRadius  <- peekM
             insideColor  <- peekM
             outsideColor <- peekM
             return (RadialGradient center
                                    innerRadius
                                    outerRadius
                                    insideColor
                                    outsideColor
                    )
  pokeM (RadialGradient center
                        innerRadius
                        outerRadius
                        insideColor
                        outsideColor
        ) = do pokeM center
               pokeM innerRadius
               pokeM outerRadius
               pokeM insideColor
               pokeM outsideColor

instance Storable (RadialGradient SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance StorableM (LinearGradient SubSpace) where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 SubSpace)
       sizeOfM (undefined :: Point2 SubSpace)
       sizeOfM (undefined :: Color          )
       sizeOfM (undefined :: Color          )
  alignmentM _ =
    do alignmentM (undefined :: Point2 SubSpace)
       alignmentM (undefined :: Point2 SubSpace)
       alignmentM (undefined :: Color          )
       alignmentM (undefined :: Color          )
  peekM = do start      <- peekM
             end        <- peekM
             startColor <- peekM
             endColor   <- peekM
             return (LinearGradient start
                                    end
                                    startColor
                                    endColor
                    )
  pokeM (LinearGradient start
                        end
                        startColor
                        endColor
        ) = do pokeM start
               pokeM end
               pokeM startColor
               pokeM endColor

instance Storable (LinearGradient SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
