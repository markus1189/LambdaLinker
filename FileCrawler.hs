module FileCrawler
  ( getRecursiveContentsP
  , expandPath
  , Predicate
  , andP, orP, notP, neitherP
  )
  where

import System.FilePath ((</>))
import Control.Monad (forM,liftM)
import qualified System.Directory as S
import System.Directory ( doesDirectoryExist
                        )

type Predicate = FilePath -> Bool


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

getRecursiveContentsP :: Predicate -> FilePath -> IO [FilePath]
getRecursiveContentsP p root = do
  root' <- expandPath root
  contents <- getDirectoryContents root
  liftM concat $ forM contents $ \name -> do
    let path = root' </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory && (doRecurse name)
      then getRecursiveContentsP p path
      else return [path]
  where doRecurse = p

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = filterD `liftM` (exPath >>= S.getDirectoryContents)
  where filterD = filter (`notElem` [".",".."])
        exPath = expandPath path

expandPath :: FilePath -> IO FilePath
expandPath "" = return ""
expandPath (x:xs)
  | x == '~' = do
    home <- S.getHomeDirectory
    return $ home ++ xs
  | otherwise = return (x:xs)

