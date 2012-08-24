module LambdaLinker
  where

import Control.Monad ( liftM
                     ,forM
                     )

import Control.Exception (handle)

import qualified System.Directory as S

import System.Directory ( doesDirectoryExist
                        , createDirectoryIfMissing
                        , getCurrentDirectory
                        )

import System.FilePath ( (</>)
                       , addTrailingPathSeparator
                       , dropExtension
                       , takeDirectory
                       )

import System.Posix.Files ( createSymbolicLink
                          , getSymbolicLinkStatus
                          )

import Data.List (isSuffixOf,sort)

main :: IO ()
main = linkRelativeFilesToHomeDir

type Predicate = FilePath -> Bool

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
    then putStrLn $ "Skipped: " ++ to
    else do
      putStrLn $ from ++ " -> " ++ to
      createDirectoryIfMissing True (takeDirectory to)
      {-createSymbolicLink from to-}

buildDestinations :: FilePath -> FilePath -> [FilePath] -> [FilePath]
buildDestinations from to = map $ buildDestination from to

buildDestination :: FilePath -> FilePath -> FilePath -> FilePath
buildDestination from to path = dropExtension $ to </> (drop (length from) path)

getFilesToLink :: FilePath -> IO [FilePath]
getFilesToLink path = sort `liftM` (filterSyms $ getRecursiveContentsP' path)
  where filterSyms = liftM $ filter extensionIsSymlink
        getRecursiveContentsP' =
          getRecursiveContentsP (extensionIsSymlink `neitherP` gitDir)

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

extensionIsSymlink :: Predicate
extensionIsSymlink = (".symlink" `isSuffixOf`)

gitDir :: Predicate
gitDir = (".git/" `isSuffixOf`) . addTrailingPathSeparator

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

doesSymlinkExist :: FilePath -> IO Bool
doesSymlinkExist path = handle onErrorFalse $
    expandPath path >>= getSymbolicLinkStatus >> return True

onErrorFalse :: IOError -> IO Bool
onErrorFalse _ = return False
