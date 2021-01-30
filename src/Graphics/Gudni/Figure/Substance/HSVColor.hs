module Graphics.Gudni.Figure.Substance.HSVColor
  ( cHue
  , cSat
  , cVal
  , rgbToHsv
  , hsvToRgb
  , hsvAdjust
  , saturate
  , shiftHue
  , lighten
  , hsvaColor
  , redish, orangeish, yellowish, greenish, blueish, purpleish
  , light
  , dark
  , veryDark
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Util.Util
import Linear.V4

import Control.Lens

cHue :: Lens' (Color s) s
cSat :: Lens' (Color s) s
cVal :: Lens' (Color s) s
cHue = cRed
cSat = cGreen
cVal = cBlue

rgbToHsv :: Space s => Color s -> Color s
rgbToHsv rgba =
    let mx = max (max (rgba ^. cRed) (rgba ^. cGreen)) (rgba ^. cBlue)
        mn = min (min (rgba ^. cRed) (rgba ^. cGreen)) (rgba ^. cBlue)
        v = mx
        d = mx - mn
        s = if mx == 0 then 0 else d / mx
        h = 1/6 * (if mx == mn
                   then 0 -- achromatic
                   else if mx == rgba ^. cRed
                        then (rgba ^. cGreen - rgba ^. cBlue) / d + (if rgba ^. cGreen < rgba ^. cBlue then 6 else 0)
                        else
                            if mx == rgba ^. cGreen
                            then (rgba ^. cBlue - rgba ^. cRed) / d + 2
                            else {-(mx == (rgba ^. cBlue))-}
                                 (rgba ^. cRed - rgba ^. cGreen) / d + 4
                  )
    in  makeColor h s v (rgba ^. cAlpha)

hsvToRgb :: Space s => Color s -> Color s
hsvToRgb hsva =
    let i = floor (hsva ^. cHue * 6)
        f = hsva ^. cHue * 6 - fromIntegral i
        p = hsva ^. cVal * (1 - hsva ^. cSat)
        q = hsva ^. cVal * (1 - f * hsva ^. cSat)
        t = hsva ^. cVal * (1 - (1 - f) * hsva ^. cSat)
        v = hsva ^. cVal
        a = hsva ^. cAlpha
    in
    case i `mod` 6 of
        0 -> makeColor v t p a
        1 -> makeColor q v p a
        2 -> makeColor p v t a
        3 -> makeColor p q v a
        4 -> makeColor t p v a
        5 -> makeColor v p q a

-- | Generate a 'Color' from hue saturation and lightness values.
hsvaColor :: (Space s) => s -> s -> s -> s -> Color s
hsvaColor h s v a = hsvToRgb $ makeColor h s v a


fract :: (Floating a, RealFrac a) => a -> a
fract x = x - (fromIntegral . floor) x

hsvAdjust :: Space s => Color s -> Color s -> Color s
hsvAdjust amountHsva rgba =
  let hsva = rgbToHsv rgba
  in
  hsvToRgb $ makeColor ( fract (hsva ^. cHue + amountHsva ^. cHue)) -- should rotate around 1.0
                       ( clamp (hsva ^. cSat + amountHsva ^. cSat) 0 1 )
                       ( clamp (hsva ^. cSat + amountHsva ^. cSat) 0 1 )
                       (hsva ^. cAlpha)


saturate :: Space s => s -> Color s -> Color s
shiftHue :: Space s => s -> Color s -> Color s
lighten  :: Space s => s -> Color s -> Color s
saturate amount rgba = hsvAdjust (makeColor 0 amount 0 0) rgba
shiftHue amount rgba = hsvAdjust (makeColor amount 0 0 0) rgba
lighten  amount rgba = hsvAdjust (makeColor 0 0 amount 0) rgba

-- | The amount to mix in a color to ish it.
ishAmount :: Space s => s
ishAmount = 0.05

influenceHue :: (Space s) => s -> Color s -> Color s -> Color s
influenceHue amount blender color =
  let blended = blend amount blender color
      (Color (V4 h s _ _)) = rgbToHsv blended
      (Color (V4 _ _ l a)) = rgbToHsv color
  in  hsvaColor h s l a

-- | Make a slightly redder version of the color.
redish    :: (Space s) => Color s -> Color s
-- | Make a slightly oranger version of the color.
orangeish :: (Space s) => Color s -> Color s
-- | Make a slightly yellower version of the color.
yellowish :: (Space s) => Color s -> Color s
-- | Make a slightly greener version of the color.
greenish  :: (Space s) => Color s -> Color s
-- | Make a slightly bluer version of the color.
blueish   :: (Space s) => Color s -> Color s
-- | Make a slightly purpler version of the color.
purpleish :: (Space s) => Color s -> Color s
redish    = influenceHue ishAmount red
orangeish = influenceHue ishAmount orange
yellowish = influenceHue ishAmount yellow
greenish  = influenceHue ishAmount green
blueish   = influenceHue ishAmount blue
purpleish = influenceHue ishAmount purple

-- | Make a slightly lighter version of the color.
light :: (Space s) => Color s -> Color s
light = saturate (-0.25) . lighten 0.2

-- | Make a slightly darker version of the color.
dark :: (Space s) => Color s -> Color s
dark  = saturate 1.25 . lighten (-0.2)

-- | Make a much darker version of the color.
veryDark :: (Space s) => Color s -> Color s
veryDark  = saturate 0.25 . lighten (-0.2)
