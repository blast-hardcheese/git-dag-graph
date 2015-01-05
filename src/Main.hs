module Main where

import Graph
import System.Process
import GHC.IO.Handle
import Git

main :: IO ()
main = do
  fname <- generateStaticGraph
  putStrLn fname

  objects <- countObjects "smoke.git"

  print objects
