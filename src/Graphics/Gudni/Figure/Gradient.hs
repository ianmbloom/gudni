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
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Transformable
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

instance Space s => SimpleTransformable (RadialGradient s) where
    translateBy v (RadialGradient center ri ro ci co) = RadialGradient (translateBy v center) ri ro ci co
    scaleBy s (RadialGradient center ri ro ci co) = RadialGradient (scaleBy s center) (ri * s) (ro * s) ci co
    stretchBy s (RadialGradient center ri ro ci co) = RadialGradient (stretchBy s center) (ri * s ^. pX) (ro * s ^. pX) ci co

instance Space s => Transformable (RadialGradient s) where
    rotateBy a (RadialGradient center ri ro ci co) = RadialGradient (rotateBy a center) ri ro ci co

instance Space s => CanProject (BezierSpace s) (RadialGradient s) where
   projectionWithStepsAccuracy = undefined

data LinearGradient s = LinearGradient
    { gradientStart      :: Point2 s
    , gradientEnd        :: Point2 s
    , gradientStartColor :: Color
    , gradientEndColor   :: Color
    } deriving (Show)

instance Space s => HasSpace (LinearGradient s) where
    type SpaceOf (LinearGradient s) = s

instance Space s => SimpleTransformable (LinearGradient s) where
    translateBy v (LinearGradient start end sc ec) = LinearGradient (translateBy v start) (translateBy v end) sc ec
    scaleBy s (LinearGradient start end sc ec) = LinearGradient (scaleBy s start) (scaleBy s end) sc ec
    stretchBy s (LinearGradient start end sc ec) = LinearGradient (stretchBy s start) (stretchBy s end) sc ec

instance Space s => Transformable (LinearGradient s) where
    rotateBy a (LinearGradient start end sc ec) = LinearGradient (rotateBy a start) (rotateBy a end) sc ec

instance Space s => CanProject (BezierSpace s) (LinearGradient s) where
   projectionWithStepsAccuracy = undefined

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
