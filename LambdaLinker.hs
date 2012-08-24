module LambdaLinker
  where

import Control.Monad (liftM,forM)

import qualified System.Directory as S
import System.Directory (doesDirectoryExist)

import System.FilePath ((</>))

getRecursiveContentsP :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getRecursiveContentsP p root = do
  contents <- getDirectoryContents root
  liftM concat $ forM contents $ \name -> do
    let path = root </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory && (doRecurse name)
      then getRecursiveContentsP p path
      else return [path]
  where doRecurse = undefined

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = filterD `liftM` (expanded >>= S.getDirectoryContents)
  where filterD = filter (`notElem` [".",".."])
        expanded = expandPath path

expandPath :: FilePath -> IO FilePath
expandPath (x:xs)
  | x == '~' = do
    home <- S.getHomeDirectory
    return $ home ++ xs
  | otherwise = return (x:xs)
