module Graphics.Gudni.Figure.Substance.HSVColor
  (
  )
where

rgbToHsv :: 
inline HsvColor_ rgbToHsv(Color_ rgba) {
    SubSpace_ mx = max(max(r,g),b);
    SubSpace_ mn = min(min(r,g),b);
    SubSpace_ h = mx;
    SubSpace_ s = mx;
    SubSpace_ v = mx;

    SubSpace_ d = mx - mn;
    s = mx == 0 ? 0 : d / mx;

    if (mx == mn) {
        h = 0; // achromatic
    } else {
        switch (mx) {
            case r: h = (g - b) / d + (g < b ? 6 : 0); break;
            case g: h = (b - r) / d + 2; break;
            case b: h = (r - g) / d + 4; break;
        }
        h = h / 6;
    }
    return (HsvColor_) (h, s, v, aCh(rgba));
}

#define hCh(hsva) hsv.s0
#define sCh(hsva) hsv.s1
#define vCh(hsva) hsv.s2
// aCh is the same as for rgba

inline Color_ hsvToRgb(HsvColor_ hsva) {
    SubSpace_ i = floor(h * 6);
    SubSpace_ f = hCh(hsva) * 6 - i;
    SubSpace_ p = vCh(hsva) * (1 - sCh(hsva));
    SubSpace_ q = vCh(hsva) * (1 - f * sCh(hsva));
    SubSpace_ t = vCh(hsva) * (1 - (1 - f) * sCh(hsva));

    switch (i % 6) {
        case 0: return (Color_) (v, t, p, aCh hsva); break;
        case 1: return (Color_) (q, v, p, aCh hsva); break;
        case 2: return (Color_) (p, v, t, aCh hsva); break;
        case 3: return (Color_) (p, q, v, aCh hsva); break;
        case 4: return (Color_) (t, p, v, aCh hsva); break;
        case 5: return (Color_) (v, p, q, aCh hsva); break;
    }
}

inline Color_ composite(Color_ foreground, Color_ background) {
  float alphaOut = ALPHA(foreground) + ALPHA(background) * (1.0f - ALPHA(foreground));
  if (alphaOut > 0) {
     Color_ color = ((foreground * ALPHA(foreground)) + (background * ALPHA(background) * (1.0f - ALPHA(foreground)))) / alphaOut;
     return (Color_) (rCh(color),gCh(color),bCh(color),alphaOut);
  }
  else {
    return CLEARBLACK;
  }
}

inline Color_ adjustHsva(HsvColor_ amountHsva, Color_ rgba) {
  HsvColor_ hsva = rgbToHsv(rgba);
  hsva = (HsvColor_) ( fract(hCh(hsva) + hCh(amountHsva))
                     , clamp(sCh(hsva) + sCh(amountHsva), 0, 1)
                     , clamp(sCh(hsva) + sCh(amountHsva), 0, 1)
                     , aCh(hsva)
                     );
  return hsvToRgb(hsva);
}

inline Color_ saturate(SubSpace_ amount, Color_ rgba) {
  return adjustHsva((HsvColor_)(0,amount,0,0),rgba);
}

inline Color_ shiftHue(SubSpace_ amount, Color_ rgba) {
  return adjustHsva((HsvColor_)(amount,0,0,0),rgba);
}

inline Color_ lighten(SubSpace_ amount, Color_ rgba) {
  return adjustHsva((HsvColor_)(0,0,amount,0),rgba);
}

inline Color_ transparent(SubSpace_ amount, Color_ rgba) {
  aCh(rgba) = amount;
  return rgba;
}
