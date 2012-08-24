module LambdaLinker
  where

import Control.Monad (liftM,forM)

import qualified System.Directory as S
import System.Directory (doesDirectoryExist)

import System.FilePath ((</>),addTrailingPathSeparator)
import Data.List (isSuffixOf)

type Predicate = FilePath -> Bool

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = filterD `liftM` (exPath >>= S.getDirectoryContents)
  where filterD = filter (`notElem` [".",".."])
        exPath = expandPath path

expandPath :: FilePath -> IO FilePath
expandPath (x:xs)
  | x == '~' = do
    home <- S.getHomeDirectory
    return $ home ++ xs
  | otherwise = return (x:xs)

extensionIsSymlink :: Predicate
extensionIsSymlink = (".symlink" `isSuffixOf`)

gitDir :: Predicate
gitDir = (".git/" `isSuffixOf`) . addTrailingPathSeparator

predicate = neitherP gitDir extensionIsSymlink

notP :: Predicate -> Predicate
notP p path = not $ p path

andP :: Predicate -> Predicate -> Predicate
andP = liftP (&&)

orP :: Predicate -> Predicate -> Predicate
orP = liftP (||)

neitherP :: Predicate -> Predicate -> Predicate
neitherP p1 p2 path = not $ (p1 path) || (p2 path)

liftP :: (Bool -> Bool -> Bool) -> Predicate -> Predicate -> Predicate
liftP f p1 p2 path = p1 path `f` p2 path
