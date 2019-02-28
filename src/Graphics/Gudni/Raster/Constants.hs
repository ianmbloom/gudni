{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Constants
    ( tILEsIZE
    , dEFAULTNUMMASKS
    , sECTIONsIZE
    , mAXsHAPE
    , mAXtHRESHOLDS
    , cONTINUATIONaLIGN
    , rANDOMFIELDsIZE
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types

tILEsIZE :: Point2 IntSpace
tILEsIZE = Point2 512 512

dEFAULTNUMMASKS :: Int
dEFAULTNUMMASKS = 16

sECTIONsIZE :: Int
sECTIONsIZE = 16

cONTINUATIONaLIGN :: CInt
cONTINUATIONaLIGN = 40

rANDOMFIELDsIZE :: Int
rANDOMFIELDsIZE = 4096

mAXtHRESHOLDS :: Int
mAXtHRESHOLDS = 512 -- the size of the threshold header and threshold geometry buffers (must be a power of 2)

mAXsHAPE :: Int
mAXsHAPE = 511 -- total number of shapes per build. must be one less than the number of bits available.
