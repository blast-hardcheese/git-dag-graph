{-# LANGUAGE OverloadedStrings #-}
module Graph where

import Data.GraphViz
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete (Attribute (Weight, Image), StyleName (Dashed), StyleItem (SItem))

gitgraph = DotGraph { strictGraph = True
                    , directedGraph = True
                    , graphID = Nothing
                    , graphStatements = DotStmts { attrStmts = []
                                                 , subGraphs = subgraphs
                                                 , nodeStmts = nodes
                                                 , edgeStmts = edges }}
  where nodes = [
-- Commits
                 DotNode "ROOT" [shape PlainText]
               , DotNode "40b1206" [shape BoxShape, textLabel "40b1206\nfirst commit"]
               , DotNode "e90d9eb" [shape BoxShape, textLabel "e90d9eb\nsecond commit", color Blue]
               , DotNode "c9b2153" [shape BoxShape, textLabel "c9b2153\nthird commit"]

-- Trees
               , DotNode "d8329fc" [shape Folder, textLabel "d8329fc\n."]
               , DotNode "0155eb4" [shape Folder, textLabel "0155eb4\n."]
               , DotNode "3c4e9cd" [shape Folder, textLabel "3c4e9cd\n."]

-- Blobs
               , DotNode "83baae6" [shape Box3D, textLabel "83baae6\ntest.txt"]
               , DotNode "83baae7" [shape Box3D, textLabel "83baae7\ntest1.txt"]
                ]
        edges = [
                  DotEdge "ROOT" "40b1206" [Weight $ Int 100]
                , DotEdge "40b1206" "d8329fc" [Weight $ Int 50]
                , DotEdge "d8329fc" "83baae6" [Weight $ Int 25]
                , DotEdge "d8329fc" "83baae7" [Weight $ Int 25]
                , DotEdge "40b1206" "e90d9eb" [Weight $ Int 100]
                , DotEdge "e90d9eb" "0155eb4" [Weight $ Int 50]
                , DotEdge "e90d9eb" "c9b2153" [Weight $ Int 100]
                , DotEdge "c9b2153" "3c4e9cd" [Weight $ Int 50]
                ]
        subgraphs = [
                      DotSG {
                        isCluster = True
                      , subGraphID = Nothing
                      , subGraphStmts = DotStmts { attrStmts = [GraphAttrs [textLabel "Orphans", style (SItem Dashed [])]]
                                                 , subGraphs = []
                                                 , nodeStmts = [
                                                   DotNode "4b825dc" [shape PlainText, Image "README_assets/cloud.png", textLabel "4b825dc\nfile.txt"]
                                                 ]
                                                 , edgeStmts = []}
                      }
                    ]

generateStaticGraph :: IO String
generateStaticGraph = do
  fname <- runGraphviz (gitgraph :: DotGraph String) DotOutput "output/thing.dot"
  runGraphviz (gitgraph :: DotGraph String) Png "output/thing.png"