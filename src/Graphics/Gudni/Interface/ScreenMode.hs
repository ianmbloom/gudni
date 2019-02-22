module Graphics.Gudni.Interface.ScreenMode
  ( ScreenMode(..)
  )
where

import Graphics.Gudni.Figure

data ScreenMode = Window (Point2 IntSpace) | FullScreen
