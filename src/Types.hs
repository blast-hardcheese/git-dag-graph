module Types where

data GitObjects = GitObjects {
        count :: Int,
        size :: Int,
        in_pack :: Int,
        packs :: Int,
        size_pack :: Int,
        prune_packable :: Int,
        garbage :: Int,
        size_garbage :: Int
    }
    deriving (Show, Eq)

type Hash = String
type Size = Int

data GitOrphan = OrphanBlob Hash | OrphanCommit Hash
    deriving (Show, Eq)

type GitOrphanList = [GitOrphan]

data GitObject = GitBlobObject Hash Size | GitCommitObject Hash Size | GitTreeObject Hash Size
    deriving (Show, Eq)

type GitObjectList = [GitObject]
