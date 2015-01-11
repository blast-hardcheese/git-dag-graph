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
  let target = "test.git"

  {-objects <- countObjects target-}
  {-print objects-}
  {-putStrLn ""-}

  {-putStrLn "findOrphans"-}
  {-orphans <- findOrphans target-}
  {-print orphans-}
  {-putStrLn ""-}

  {-putStrLn "getAllObjectHashes"-}
  files <- getAllObjectHashes target
  {-print files-}
  {-putStrLn ""-}

  {-putStrLn "objectHashesToObjects"-}
  (Right objects) <- objectHashesToObjects target files
  {-print objects-}
  {-putStrLn ""-}

  (treePairs, orphans) <- extractTrees target objects
  (commitPairs, orphans) <- extractCommits target orphans

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

  let commitNodesAndEdges = commitPairToNodesAndEdges <$> commitPairs
  let commitNodes = fst <$> commitNodesAndEdges
  let commitEdges = concat $ snd <$> commitNodesAndEdges

  let nodes = treeNodes ++ commitNodes
  let edges = treeEdges ++ commitEdges

  let graph = DotGraph { strictGraph = True
                       , directedGraph = True
                       , graphID = Nothing
                       , graphStatements = DotStmts { attrStmts = []
                                                    , subGraphs = [orphanGraph]
                                                    , nodeStmts = nodes
                                                    , edgeStmts = edges }}

  fname <- generateGraph $ graph
  putStrLn fname
