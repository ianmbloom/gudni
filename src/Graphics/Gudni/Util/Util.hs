{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DoAndIfThenElse       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.Util
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Various utility functions used in the library.

module Graphics.Gudni.Util.Util
  ( takeItem
  , removeItem
  , breakList
  , breakVector
  , adjustedLog
  , mapFst
  , mapSnd
  , mapOne
  , mapTwo
  , mapThree
  , mapFstOf4
  , mapPair
  , notLast
  , takeFirstLast
  , splitLast
  , mapMap
  , splitWhen
  , clamp
  , number
  , fmapMaybe
  , xorMaybe
  , wrapMaybe
  , TimeKeeper
  , startTimeKeeper
  , markTime
  , showTimes
  , withLift
  , withIO
  , lpad
  , rpad
  , putStrList
  , orNothing
  , titleLine
  , infoLine
  , eitherMaybe
  , firstMaybe
  , secondMaybe
  , const2
  , const3
  , nullTail
  , nullHead
  , partitionM
  , showAsLetters
  , overStateT
  )
where

import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Lens
import Control.Applicative

import qualified Data.Vector as V
import Data.Time.Clock
import Data.Char

data TimeKeeper = TimeKeeper
  { tKStartTime :: UTCTime
  , tKLastTime  :: UTCTime
  , tKMarkers   :: [(String, NominalDiffTime)]
  }

startTimeKeeper =
  do
    startTime <- getCurrentTime
    return TimeKeeper {tKStartTime = startTime, tKLastTime = startTime, tKMarkers = []}

markTime :: String -> TimeKeeper -> IO TimeKeeper
markTime name tk =
  do
    currentTime <- getCurrentTime
    let interval = diffUTCTime currentTime (tKLastTime tk)
    let newMarkers = (name, interval) : tKMarkers tk
    return tk {tKLastTime = currentTime, tKMarkers = newMarkers}

showTimes sectionName showFps tk =
  let buffer text = replicate (20 - length text) ' '
      shower (name, interval) = buffer name ++ name ++ ": " ++ showFlFixed' 1 3 (realToFrac $ toRational interval)
      totalInterval = diffUTCTime (tKLastTime tk) (tKStartTime tk)
      times = map shower . reverse $ tKMarkers tk
      header = [sectionName]
      total = [shower ("Total", totalInterval)]
      fps = if showFps then ["---- Possible FPS "++ showFl ( 1 / realToFrac totalInterval) ++ "----"] else []
  in  unlines ({-header ++-} times ++ total ++ fps)


-- | Return the ceiling value of a log2 x adjusted to zero for x < 1.
adjustedLog :: (Integral s, Integral v )=> s -> v
adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

-- Utility function, returns the ith element of a list as well as the list without it.

mapFst f (x,y) = (f x, y)
mapSnd f (x,y) = (x, f y)
mapOne f (x, y, z) = (f x, y, z)
mapTwo f (x, y, z) = (x, f y, z)
mapThree f (x, y, z) = (x, y, f z)

mapFstOf4 f (x, y, z, w) = (f x, y, z, w)

mapPair f (a,b) = (f a, f b)

breakList :: Int -> [a] -> [[a]]
breakList i xs = let (front, rest) = splitAt i xs in
                     case rest of
                       [] -> pure front
                       xxs -> front:breakList i rest

-- | Split a vector into a list over vectors with a maximum size.
breakVector :: Int -> V.Vector a -> V.Vector (V.Vector a)
breakVector maxSize vector = if V.length vector > maxSize
                               then let (first, rest) = V.splitAt maxSize vector
                                    in first `V.cons` breakVector maxSize rest
                               else pure vector

takeItem :: Int -> [a] -> (a, [a])
takeItem i list =
  case splitAt i list of
    (h,x:xs) -> (x, h ++ xs)
    (h,[]  ) -> error "index out of bounds on list"

removeItem i ss = snd $ takeItem i ss

splitWhen :: (a -> Bool) -> [a] -> Maybe ([a], [a])
splitWhen condition = splitWhen' condition []

splitWhen' condition ls (x:xs) = if condition x then Just (ls,xs) else splitWhen' condition (ls ++ [x]) xs
splitWhen' condition ls []     = Nothing

-- everything but the last element
notLast :: [a] -> [a]
notLast [x]    = []
notLast (x:xs) = x : notLast xs
notLast []     = error "notLast cannot be empty"

takeFirstLast :: [a] -> (a, [a], a)
takeFirstLast xs = let (center, right) = splitLast (tail xs) in (head xs, center, right)

splitLast :: [a] -> ([a],a)

splitLast [y] = ([],y)
splitLast (x:xs) =
  let (xss,y) = splitLast xs
  in (x:xss,y)
splitLast [] = error "splitLast must have at least one element"

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap = map . map

{-# INLINE clamp #-}
clamp :: Ord r => r -> r -> r -> r
clamp bottom top n = min top $ max bottom n

number :: [a] -> [(Int, a)]
number = zip [0..]

xorMaybe :: Maybe a -> Maybe a -> Maybe a
xorMaybe (Just x) Nothing  = Just x
xorMaybe Nothing  (Just y) = Just y
xorMaybe Nothing  Nothing  = Nothing
xorMaybe (Just x) (Just y) = error "xorMaybe found two values."

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = fmap -- this is just here to force the selection of the right monad.

wrapMaybe :: (a -> Bool) -> a -> Maybe a
wrapMaybe cond x = if cond x then Just x else Nothing

withLift :: (MonadState p m, Monad m) => (m1 (b, s) -> m (b, s)) -> Lens' p s -> StateT s m1 b -> m b
withLift lifter lens mf =
  do state <- use lens
     (result, state') <- lifter $ runStateT mf state
     lens .= state'
     return result

withIO :: (MonadState p m, MonadIO m) => Lens' p s -> StateT s IO b -> m b
withIO lens mf =
  do state <- use lens
     (result, state') <- liftIO $ runStateT mf state
     lens .= state'
     return result

lpad :: Int -> String -> String
lpad i m = let l = i - length m
           in replicate l ' ' ++ m

rpad :: Int -> String -> String
rpad i m = let l = i - length m
           in m ++ replicate l ' '

putStrList :: (Show a) => [a] -> IO ()
putStrList ls =
  do
    putStrLn "["
    forM_ ls $ \ x ->
      putStrLn $ "   " ++ show x
    putStrLn "]"

orNothing :: Bool -> b -> Maybe b
orNothing cond val = if cond then Just val else Nothing

-- | Show a title line
titleLine :: String -> String
titleLine title = "======================== " ++ title ++ " ========================\n"
-- | Pad a line of info for aligned display.
infoLine :: (String, String) -> String
infoLine (title, info) = (lpad 40 title) ++ ": " ++ info ++ "\n"

eitherMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
eitherMaybe f a b = f <$> a <*> b <|> a <|> b
firstMaybe  f a b = f <$> a <*> b <|> a
secondMaybe f a b = f <$> a <*> b       <|> b

const2 :: a -> b -> c -> c
const2 a b c = c

const3 :: a -> b -> c -> d -> d
const3 a b c d = d

nullTail x = if null x then x else tail x
nullHead x = if null x then [] else [head x]
{-
mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f a l = swap <$> runStateT (mapM go l) a
    where
        go i = do
            s <- get
            (s', r) <- lift $ f s i
            put s'
            return r
-}

partitionM ::  Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = foldM (selectM p) ([],[]) xs

selectM :: Monad m => (a -> m Bool) -> ([a], [a]) -> a -> m ([a], [a])
selectM p  ~(ts,fs) x =
         do px <- p x
            if px
            then return (x:ts,fs)
            else return (ts, x:fs)

showAsLetters :: (Num i, Integral i) => i -> String
showAsLetters i =
  let rem = i `mod` 26
      den = i `div` 26
      pre = if den > 0 then showAsLetters den else ""
  in
  pre ++ (pure $ chr $ fromIntegral $ rem + (fromIntegral . ord $ 'A'))

overStateT :: (MonadState g m) => Lens' g a -> (StateT a m b) -> m b
overStateT lens code =
  do  state <- use lens
      (b, state') <- runStateT code state
      lens .= state'
      return b
