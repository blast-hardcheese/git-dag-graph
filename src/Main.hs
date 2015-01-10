module Main where

import Control.Applicative ((<$>))
import Graph
import System.Process
import GHC.IO.Handle
import Git
import Types
import Data.Maybe (listToMaybe, fromMaybe)

main :: IO ()
main = do
  fname <- generateStaticGraph
  putStrLn fname

  objects <- countObjects "test.git"
  print objects

  putStrLn "findOrphans"
  orphans <- findOrphans "test.git"
  print orphans

  putStrLn "getAllObjectHashes"
  files <- getAllObjectHashes "test.git"
  print files

  putStrLn "objectHashesToObjects"
  (Right objects) <- objectHashesToObjects "test.git" files
  print objects

  let nonEmptyTree = head $ filter (\x -> case x of { (GitTreeObject h s) -> s /= 0; _ -> False }) objects
  putStrLn "lsTree"
  print nonEmptyTree
  res <- lsTree "test.git" $ getHash nonEmptyTree
  print res
