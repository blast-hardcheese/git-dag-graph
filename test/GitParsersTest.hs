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
                  \size-garbage: 0\
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
                  \size: 48\
                  \"
      parseGitObjects input `shouldBe` (Right $ GitObjects 12 48 13 1 6487 0 0 0)
