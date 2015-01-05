module Main where

import Types
import GitParsers

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseGitObjects" $ do
    it "correctly parses input" $ do
      let input = "\
                  \count: 12\n\
                  \size: 48\n\
                  \in-pack: 13\n\
                  \packs: 1\n\
                  \size-pack: 6487\n\
                  \prune-packable: 0\n\
                  \garbage: 0\n\
                  \size-garbage: 0\n\
                  \"
      parseGitObjects input `shouldBe` (Right $ GitObjects 12 48 13 1 6487 0 0 0)

    it "correctly parses shuffled input" $ do
      let input = "\
                  \packs: 1\n\
                  \size-pack: 6487\n\
                  \in-pack: 13\n\
                  \garbage: 0\n\
                  \prune-packable: 0\n\
                  \count: 12\n\
                  \size-garbage: 0\n\
                  \size: 48\n\
                  \"
      parseGitObjects input `shouldBe` (Right $ GitObjects 12 48 13 1 6487 0 0 0)

  describe "parseGitOrphanList" $ do
    it "should correctly parse output" $ do
      let input = "\
                   \unreachable blob 1821e36ced9ba346c4658e0bf649a30f0a86b59f\n\
                   \unreachable blob 369892cda5a6934f6ef485e82fb956cb534c99ea\n\
                   \unreachable commit 413179ea28070b6a69c2375e50acc596fb3bc020\n\
                   \unreachable blob 43a0075bc84a93134af10a07606cda44ad2af0ab\n\
                   \unreachable commit 63de624e32ce09bbf37a42eb3359ac610983c203\n\
                   \unreachable blob 9b7dbe44ec378d0f14480298f559f6c2e439c688\n\
                   \unreachable blob b10da2f8157583c7d88a1624792140356bd8076e\n\
                   \unreachable blob c3d436fda7080e4bb3e28e1add2bc1ff1b56b0ec\n\
                   \unreachable blob c4ee07a2fe4a30c327d9f737dba09a22465818a6\n\
                   \unreachable blob d3ab854dcdb072141f83ff9b7f0cb0390981f918\n\
                   \unreachable blob e69de29bb2d1d6434b8b29ae775ad8c2e48c5391\n\
                   \"

      let expected = [
                      OrphanBlob "1821e36ced9ba346c4658e0bf649a30f0a86b59f",
                      OrphanBlob "369892cda5a6934f6ef485e82fb956cb534c99ea",
                      OrphanCommit "413179ea28070b6a69c2375e50acc596fb3bc020",
                      OrphanBlob "43a0075bc84a93134af10a07606cda44ad2af0ab",
                      OrphanCommit "63de624e32ce09bbf37a42eb3359ac610983c203",
                      OrphanBlob "9b7dbe44ec378d0f14480298f559f6c2e439c688",
                      OrphanBlob "b10da2f8157583c7d88a1624792140356bd8076e",
                      OrphanBlob "c3d436fda7080e4bb3e28e1add2bc1ff1b56b0ec",
                      OrphanBlob "c4ee07a2fe4a30c327d9f737dba09a22465818a6",
                      OrphanBlob "d3ab854dcdb072141f83ff9b7f0cb0390981f918",
                      OrphanBlob "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
                    ]

      parseGitOrphanList input `shouldBe` (Right expected)
