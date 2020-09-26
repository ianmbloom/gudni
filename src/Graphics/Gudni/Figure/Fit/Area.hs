module Graphics.Gudni.Figure.Fit.Area
  (
  )
where

areaList :: s -> [s] -> [s] -> s
areaList gap =
  let section a b = gap * abs (a - b)
  in  zipWith section

areaBetween :: (s ->
