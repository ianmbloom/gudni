{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Experimental.ConfineTreeTest
  ( --runLessCurveTests
    runCrossTests
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Experimental.ConstructConfineQuery
import Control.Lens

showCross :: (Axis axis, IsStyle style, LessPoint axis)
          => axis
          -> Point2 (SpaceOf style)
          -> Point2 (SpaceOf style)
          -> Bezier (SpaceOf style)
          -> Layout style
showCross axis start end bez =
    let doesCross = crossesAlong axis (start ^. athwart axis) (start ^. along axis) (end ^. along axis) bez
        color = if doesCross then red else green
    in  withColor color     . mask . stroke 0.1 . makeOpenCurve $ [line start end]

overCs :: (Point2 s -> Point2 s) -> (EitherAxis, Point2 s, Point2 s) -> (EitherAxis, Point2 s, Point2 s)
overCs f (x, a, b) = (x, f a, f b)

crossTests :: forall style
           .  (IsStyle style)
           => Bezier (SpaceOf style)
           -> Layout style
crossTests bez =
   let s :: SpaceOf style
       s = 50
       cs :: [(EitherAxis, Point2 (SpaceOf style), Point2 (SpaceOf style))]
       cs = [ (Right Vertical,  Point2 0   0,   Point2 0   1  )
            , (Left Horizontal, Point2 0   1,   Point2 1   1  )
            , (Right Vertical,  Point2 1   1,   Point2 1   0  )
            , (Left Horizontal, Point2 1   0,   Point2 0   0  )
            , (Right Vertical,  Point2 0.5 0,   Point2 0.5 1  )
            , (Left Horizontal, Point2 0 0.5,   Point2 1   0.5)
            , (Right Vertical,  Point2 0.1 0.1, Point2 0.1 0.9)
            , (Left Horizontal, Point2 0.1 0.9, Point2 0.9 0.9)
            , (Right Vertical,  Point2 0.9 0.9, Point2 0.9 0.1)
            , (Left Horizontal, Point2 0.9 0.1, Point2 0.1 0.1)

            , (Right Vertical,  Point2 0 (-0.5),Point2 0   0  )
            , (Left  Horizontal,Point2 (-0.5) 0,Point2 0   0  )

            , (Right Vertical,   Point2 0 1, Point2 0      1.5)
            , (Left  Horizontal, Point2 0 1, Point2 (-0.5) 1  )

            , (Right Vertical,   Point2 1 1, Point2 1 1.5)
            , (Left  Horizontal, Point2 1 1, Point2 1.5 1)

            , (Right Vertical,   Point2 1 0, Point2 1 (-0.5))
            , (Left  Horizontal, Point2 1 0, Point2 (1.5) 0)
            ]
       reBez :: Bezier (SpaceOf style)
       reBez = overBezier (^* s) bez

       zs :: [(EitherAxis, Point2 (SpaceOf style), Point2 (SpaceOf style))]
       zs = map (overCs (^* s)) cs
   in  overlap $ (withColor black . mask . stroke 0.1 $ makeOpenCurve [reBez]):
                 map (\(e, start, end) ->
                       withEitherAxis showCross showCross e start end reBez) zs

runCrossTests :: Layout DefaultStyle
runCrossTests =
  let bez0 = Bez (Point2 0 0) (Point2 0.5 0.5) (Point2 1 1)
      bez1 = Bez (Point2 0 1) (Point2 0.5 0.5) (Point2 1 0)
      bez2 = Bez (Point2 0 0.5) (Point2 0.5 0.5) (Point2 1 0.5)
      bez3 = Bez (Point2 0.5 0) (Point2 0.5 0.5) (Point2 0.5 1)

      bez4 = Bez (Point2 0 0) (Point2 0 1) (Point2 1 1)
      bez5 = Bez (Point2 0 0) (Point2 1 0) (Point2 1 1)
      bez6 = Bez (Point2 1 0) (Point2 1 1) (Point2 0 1)
      bez7 = Bez (Point2 1 0) (Point2 0 0) (Point2 0 1)
  in  rack [ stack [ crossTests bez0
                   , crossTests bez1
                   , crossTests bez2
                   , crossTests bez3
                   ]
           , stack [ crossTests bez4
                   , crossTests bez5
                   , crossTests bez6
                   , crossTests bez7
                   ]
           ]
