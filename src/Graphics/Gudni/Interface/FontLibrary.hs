{-# LANGUAGE CPP #-}
module Graphics.Gudni.Interface.FontLibrary
  ( fontLibrary
  )
where

import System.Info
import Control.Monad
import Data.List

import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, getDirectoryContents)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

absolutize :: String -> IO String
absolutize aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = return aPath

fontDirectories =
  case os of
    "darwin" -> mapM absolutize ["~/Library/Fonts/", "/Library/Fonts/"]
    _        -> return ["C:\\windows\\fonts\\"]

absoluteDirectoryContents dir =
  do files <- getDirectoryContents dir
     return $ map (addTrailingPathSeparator dir ++) files

fontLibrary :: IO [String]
fontLibrary =
  do dirs <- fontDirectories
     files <- concat <$> mapM absoluteDirectoryContents dirs
     return $ filter (isSuffixOf "ttf") files
