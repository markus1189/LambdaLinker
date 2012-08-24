module Main where

import System.FilePath ( addTrailingPathSeparator )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )
import LambdaLinker ( linkRelativeFilesToHomeDir
                    , linkFiles
                    , getFilesToLink
                    )

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()

handleArgs ("link":from:to:[]) = do
  putStrLn $ "Linking files from '" ++ from ++ "'"
  linkFiles (addTrailingPathSeparator from) (addTrailingPathSeparator to)

handleArgs ("link":[]) = do
      putStrLn "Linking relative files"
      linkRelativeFilesToHomeDir

handleArgs ("list":from:[]) = do
  putStrLn "Files to link:"
  files <- (getFilesToLink . addTrailingPathSeparator) from
  mapM_ putStrLn files

handleArgs ("list":[]) = do
  dir <- getCurrentDirectory
  files <- (getFilesToLink . addTrailingPathSeparator) dir
  mapM_ putStrLn files

handleArgs _ = putStrLn $ "Invalid args.\n" ++
                          "lambdaLinker list [path]\t->\tList files to link\n" ++
                          "lambdaLinker link [from to]\t->\tLink files"
