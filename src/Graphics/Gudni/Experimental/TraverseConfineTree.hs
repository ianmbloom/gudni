  , Layer(..)
  , layerSubstance
  , layerInside
  , layerCurve
  , SimpleLayer(..)
  , simpSubstance
  , simpInverse


data Layer s =
  Layer
  { _layerSubstance :: SubstanceTagId
  , _layerInside    :: Bool
  , _layerCurve     :: Bezier s
  } deriving (Show)
makeLenses ''Layer

data SimpleLayer =
  SimpleLayer
  { _simpSubstance :: SubstanceTagId
  , _simpInverse   :: Bool
  } deriving (Show)
makeLenses ''SimpleLayer

simplifyLayer :: Space s => Point2 s -> Layer s -> SimpleLayer
simplifyLayer point layer =
  let aboveCurve = isAboveCurve point (layer ^. layerCurve)
  in  SimpleLayer (layer ^. layerSubstance) (aboveCurve == layer ^. layerInside)

sampleStack :: Space s => Point2 s -> [Layer s] -> [SimpleLayer]
sampleStack point = map (simplifyLayer point) . sortWith (view layerSubstance)
