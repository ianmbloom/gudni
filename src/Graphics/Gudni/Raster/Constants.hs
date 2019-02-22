{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Constants
    ( tILEsIZE
    , dEFAULTNUMMASKS
    , sECTIONsIZE
    , cONTINUATIONaLIGN
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types

tILEsIZE :: Point2 IntSpace
tILEsIZE = makePoint 512 512

dEFAULTNUMMASKS :: Int
dEFAULTNUMMASKS = 16

sECTIONsIZE :: Int
sECTIONsIZE = 16

cONTINUATIONaLIGN :: CInt
cONTINUATIONaLIGN = 40
