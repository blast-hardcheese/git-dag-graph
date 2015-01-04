module Main where

import Graph

main :: IO ()
main = do
  fname <- generateStaticGraph
  putStrLn fname
