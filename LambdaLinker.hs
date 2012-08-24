module LambdaLinker
  ( linkRelativeFilesToHomeDir
  , linkFiles
  , getFilesToLink
  )
  where

import Control.Monad ( liftM )

import Control.Exception (handle)

import System.Directory ( createDirectoryIfMissing
                        , getCurrentDirectory
                        )

import System.FilePath ( (</>)
                       , addTrailingPathSeparator
                       , dropExtension
                       , takeDirectory
                       )

import System.Posix.Files ( getSymbolicLinkStatus
                          , createSymbolicLink
                          )

import Data.List (isSuffixOf,sort)

import FileCrawler ( getRecursiveContentsP
                   , expandPath
                   , Predicate
                   , neitherP
                   )

linkRelativeFilesToHomeDir :: IO ()
linkRelativeFilesToHomeDir = do
  curDir <- getCurrentDirectory
  linkFiles (addTrailingPathSeparator curDir) "~/"

linkFiles :: FilePath -> FilePath -> IO ()
linkFiles from to = do
  fromE <- expandPath from
  toE <- expandPath to
  files <- getFilesToLink fromE
  let args = files `zip` (buildDestinations fromE toE files)
  mapM_ (uncurry linkDotfile) args

linkDotfile :: FilePath -> FilePath -> IO ()
linkDotfile from to = do
  symlinkExists <- doesSymlinkExist to
  if symlinkExists
    then return ()
    else do
      putStrLn $ from ++ " -> " ++ to
      createDirectoryIfMissing True (takeDirectory to)
      createSymbolicLink from to

buildDestinations :: FilePath -> FilePath -> [FilePath] -> [FilePath]
buildDestinations from to = map $ buildDestination from to

buildDestination :: FilePath -> FilePath -> FilePath -> FilePath
buildDestination from to path = dropExtension $ to </> (drop (length from) path)

getFilesToLink :: FilePath -> IO [FilePath]
getFilesToLink path = sort `liftM` (filterSyms $ getRecursiveContentsP' path)
  where filterSyms = liftM $ filter extensionIsSymlink
        getRecursiveContentsP' =
          getRecursiveContentsP (extensionIsSymlink `neitherP` gitDir)

doesSymlinkExist :: FilePath -> IO Bool
doesSymlinkExist path = handle onErrorFalse $
    expandPath path >>= getSymbolicLinkStatus >> return True

onErrorFalse :: IOError -> IO Bool
onErrorFalse _ = return False

extensionIsSymlink :: Predicate
extensionIsSymlink = (".symlink" `isSuffixOf`)

gitDir :: Predicate
gitDir = (".git/" `isSuffixOf`) . addTrailingPathSeparator
