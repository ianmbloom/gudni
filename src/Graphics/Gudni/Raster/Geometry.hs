module Graphics.Gudni.Raster.Geometry
  ( GeometryPile(..)
  , buildGeometryPile
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Pile

import Control.Monad
import Control.Monad.State
import Foreign.Storable

type GeometryPile = BytePile
-- add the enclosure data to the geometry pile
appendGeoRef :: Enclosure
             -> StateT GeometryPile IO GeoReference
appendGeoRef enclosure =
    do  geometryPile <- get
        (pile', offsetShapeStartBytes) <- liftIO $ addToBytePile "appendGeoRef" geometryPile enclosure
        put pile'
        -- the size of the shape data is measured in 64 bit chunks so that a short int can address more data.
        let offsetShapeStart = Ref $ fromIntegral offsetShapeStartBytes `div` fromIntegral (sizeOf (undefined :: Point2 DisplaySpace) * 2)
        return $ GeoRef offsetShapeStart (enclosureNumStrands enclosure)

makePrimEntry :: (BoundingBox, Enclosure)
              -> StateT GeometryPile IO PrimEntry
makePrimEntry (box, enclosure) =
    do  -- append the geometric enclosure data to the heap and return a reference
        geoRef <- appendGeoRef enclosure
        return $ PrimEntry geoRef (enclosureNumStrands enclosure) box

overShaper :: (t -> StateT s IO u) -> (Shaper t) -> StateT s IO (Shaper u)
overShaper f (Shaper i t) = do u <- f t
                               return $ Shaper i u

buildGeometryPile :: [Shaper (BoundingBox, Enclosure)] -> IO ([Shaper PrimEntry], GeometryPile)
buildGeometryPile boundedShapedEnclosures =
  do geometryPile <- newPileSize 65536 :: IO BytePile
     runStateT (mapM (overShaper makePrimEntry) boundedShapedEnclosures) geometryPile

outputGeometryPile :: GeometryPile -> IO ()
outputGeometryPile pile =
  do
    putStrLn "---------------- rJGeometryPile ------------------- "
    print pile
    putStr =<< fmap unlines (bytePileToGeometry pile)
