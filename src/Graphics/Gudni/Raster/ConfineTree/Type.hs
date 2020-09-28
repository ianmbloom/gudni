{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.Gudni.Raster.ConfineTree.Type
  ( ConfineTree (..)
  , Confine(..)
  , Branch(..)
  , confineItemTagId
  , confineCurveTag
  , confineCurve
  , confineCut
  , confineCrossings
  --, confineCrossedCurves
  , confineConsidered
  , confineOverhang
  , confineLessCut
  , confineMoreCut
  , With(..)
  , onAxis
  , fromAxis
  , CurveTag(..)
  , pointFromAxis
  , toggleCrossing
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.Util(breakVector, clamp)
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Lens
import Data.Kind
import Data.Char

import qualified Data.Vector as V
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

newtype With axis s = With {unAxis :: s} deriving (Generic, Eq, Ord)

instance (Show s, Axis axis) => Show (With axis s) where
  show (With s) = showSymbol (undefined :: axis) ++ show s

onAxis :: axis -> s -> With axis s
onAxis axis s = With s

instance (Out s, Axis axis, Show s) => Out (With axis s) where
      doc (With x) = text (showSymbol (undefined :: axis)) <> (text . show $ x)
      docPrec _ = doc

fromAxis :: axis -> With axis s -> s
fromAxis axis (With s) = s

newtype CurveTag = CurveTag Int deriving (Num, Eq, Ord)

instance Show CurveTag where
  show (CurveTag i) =
    let rem = i `mod` 26
        den = i `div` 26
        pre = if den > 0 then show (CurveTag den) else ""
    in
    pre ++ (pure $ chr $ rem + (ord 'A'))

instance Out CurveTag where
  doc x = text (show x)
  docPrec _ = doc

data Confine axis s
     = Confine
     { _confineCurveTag  :: CurveTag
     , _confineCurve     :: Bezier s
     , _confineItemTagId :: ItemTagId
     , _confineCrossings :: [ItemTagId]
     --, _confineCrossedCurves :: [CurveTag]
     , _confineConsidered:: Int
     , _confineCut       :: With axis s
     , _confineOverhang  :: With axis s
     , _confineLessCut   :: Maybe (Confine (NextAxis axis) s)
     , _confineMoreCut   :: Maybe (Confine (NextAxis axis) s)
     }
     deriving (Generic)
makeLenses ''Confine

deriving instance (Axis axis, Show s) => Show (Confine axis s)

--instance (Out s, Axis axis, Show s) => Out (Confine axis s)

instance (Space s, Axis axis, Out s, Out axis, Out (NextAxis axis), axis~NextAxis(NextAxis axis)) => Out (Confine axis s)
   where
    doc tree =
        ( doc (tree ^. confineCurveTag) <+>
          doc (tree ^. confineItemTagId) <+>
          doc (tree ^. confineCut) <+>
          doc (tree ^. confineOverhang) <+>
          doc (tree ^. confineCrossings) <+>
          --doc (tree ^. confineCrossedCurves) <+>
          text "?>" <+>
          doc (tree ^. confineConsidered)
          --text (show (tree ^. confineCurve))
         )
         $$
         nest 1 ( outMConfineTree (tree ^. confineLessCut) $$ outMConfineTree (tree ^. confineMoreCut))
    docPrec _ = doc

outMConfineTree mTree =
        case mTree of
          Nothing -> text "X"
          Just tree -> doc tree


type Branch axis s = Maybe (Confine axis s)
type ConfineTree s = Branch Vertical s

pointFromAxis :: (Axis axis, Space s) => axis -> With axis s -> With (NextAxis axis) s -> Point2 s
pointFromAxis axis parentLine parentCut =
    set (athwart axis)  (fromAxis axis            parentLine) .
    set (along axis) (fromAxis (nextAxis axis) parentCut) $
    zeroPoint

eitherConfine :: Lens' (Confine Vertical s) x -> Lens' (Confine Horizontal s) x -> Either (Confine Vertical s) (Confine Horizontal s) -> x
eitherConfine a b (Left  confine) = confine ^. a
eitherConfine a b (Right confine) = confine ^. b

toggleCrossing :: ItemTagId -> [ItemTagId] -> [ItemTagId]
toggleCrossing itemTagId stack =
  case stack of
    (x:xs) | itemTagId <  x -> itemTagId:x:xs
           | itemTagId == x -> xs
           | itemTagId >  x -> x:toggleCrossing itemTagId xs
    [] -> [itemTagId]
