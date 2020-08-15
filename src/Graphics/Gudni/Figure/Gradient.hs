{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Gradient
    ( RadialGradient(..)
    , LinearGradient(..)
    )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Color
import Control.DeepSeq
import Graphics.Gudni.Util.StorableM
import Control.Lens

data RadialGradient s = RadialGradient
    { gradientCenter       :: Point2 s
    , gradientInnerRadius  :: s
    , gradientOuterRadius  :: s
    , gradientInsideColor  :: Color
    , gradientOutsideColor :: Color
    } deriving (Show)

instance Space s => HasSpace (RadialGradient s) where
    type SpaceOf (RadialGradient s) = s

data LinearGradient s = LinearGradient
    { gradientStart      :: Point2 s
    , gradientEnd        :: Point2 s
    , gradientStartColor :: Color
    , gradientEndColor   :: Color
    } deriving (Show)

instance Space s => HasSpace (LinearGradient s) where
    type SpaceOf (LinearGradient s) = s

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

instance NFData s => NFData (LinearGradient s) where
  rnf (LinearGradient a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
instance NFData s => NFData (RadialGradient s) where
  rnf (RadialGradient a b c d e ) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` ()
