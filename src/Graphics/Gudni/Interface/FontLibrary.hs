
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Graphics.Gudni.Interface.FontLibrary
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for finding and loading fonts on the host system.

module Graphics.Gudni.Interface.FontLibrary
  ( fontLibrary
  , findDefaultFont
  )
where

import System.Info
import System.Exit
import Control.Monad
import Data.List

import Data.Foldable
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, getDirectoryContents)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.List (isPrefixOf)
import Data.Monoid (First(..))
import Graphics.Text.TrueType (buildCache, enumerateFonts, findFontInCache, FontDescriptor(..), FontCache(..), FontStyle(..))


findDefaultFont :: IO String
findDefaultFont = fromMaybe "Times New Roman.ttf" <$> listToMaybe . filter (isInfixOf "Times New Roman.ttf") <$> fontLibrary

-- | Make a relative path absolute on MacOS.
absolutizeMacPath :: String -> IO String
absolutizeMacPath aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ tail aPath
    | otherwise = return aPath

-- | Get the default font director based on the host operating system.
fontDirectories :: IO [String]
fontDirectories =
  case os of
    "darwin" -> mapM absolutizeMacPath ["~/Library/Fonts/", "/Library/Fonts/", "/System/Library/Fonts/Supplemental/"]
    _        -> return ["C:\\windows\\fonts\\"]

-- | Get the absolute contents of a director.
absoluteDirectoryContents :: FilePath -> IO [String]
absoluteDirectoryContents dir =
  do files <- getDirectoryContents dir
     return $ map (addTrailingPathSeparator dir ++) files

-- | Return a list of loadable font files on the system.
fontLibrary :: IO [String]
fontLibrary =
  do dirs <- fontDirectories
     files <- concat <$> mapM absoluteDirectoryContents dirs
     return $ filter (isSuffixOf "ttf") files

{-
findDefaultFont :: IO String
findDefaultFont = do
    cache <- buildCache
    -- Try several families in order of preference
    let roman family = FontDescriptor family (FontStyle False False)
    let m_font = getFirst $ foldMap First $
            [findFontInCache cache (roman family) | family <- [ "Times New Roman", "Liberation Serif" ] ]
    case m_font of
        Just font -> return font
        Nothing -> case enumerateFonts cache of
            [] -> die "could not find any fonts"
            all@(first : _) -> do
                putStrLn "Did not find known font.  withing first from available fonts:"
                traverse_ print all
                return (fromJust (findFontInCache cache first))
-- | Return a list of loadable font files on the system.
fontLibrary :: IO [String]
fontLibrary = do
    cache <- buildCache
    return $ map (fromJust . findFontInCache cache) (enumerateFonts cache)
-}
