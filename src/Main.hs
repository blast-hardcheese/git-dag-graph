{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Graph
import System.Process
import GHC.IO.Handle
import Git
import Types
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text.Lazy as T

import Data.GraphViz
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete (Attribute (Weight, Image), StyleName (Dashed), StyleItem (SItem))

main :: IO ()
main = do
  fname <- generateStaticGraph
  putStrLn fname

  {-objects <- countObjects "test.git"-}
  {-print objects-}
  {-putStrLn ""-}

  {-putStrLn "findOrphans"-}
  {-orphans <- findOrphans "test.git"-}
  {-print orphans-}
  {-putStrLn ""-}

  {-putStrLn "getAllObjectHashes"-}
  files <- getAllObjectHashes "test.git"
  {-print files-}
  {-putStrLn ""-}

  {-putStrLn "objectHashesToObjects"-}
  (Right objects) <- objectHashesToObjects "test.git" files
  {-print objects-}
  {-putStrLn ""-}

  (treePairs, orphans) <- extractTrees "test.git" objects
  (commitPairs, orphans) <- extractCommits "test.git" orphans

  let orphanNodes = ((objectToNode BoxShape "?") <$> orphans)

  let orphanGraph = DotSG { isCluster = True
                          , subGraphID = Just $ Str "Orphans"
                          , subGraphStmts = DotStmts { attrStmts = [GraphAttrs [textLabel "Orphans", style (SItem Dashed [])]]
                                                     , subGraphs = []
                                                     , nodeStmts = orphanNodes
                                                     , edgeStmts = [] }}

  let treeNodesAndEdges = treePairToNodesAndEdges <$> treePairs
  let treeNodes = concat $ fst <$> treeNodesAndEdges
  let treeEdges = concat $ snd <$> treeNodesAndEdges

  let nodes = treeNodes
  let edges = treeEdges

  let graph = DotGraph { strictGraph = True
                       , directedGraph = True
                       , graphID = Nothing
                       , graphStatements = DotStmts { attrStmts = []
                                                    , subGraphs = [orphanGraph]
                                                    , nodeStmts = nodes
                                                    , edgeStmts = edges }}

  fname <- generateGraph $ graph
  putStrLn fname
