module Types where

data GitObjectStats = GitObjectStats {
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
type ShortHash = String

type Size = Int

data GitOrphan = OrphanBlob Hash | OrphanCommit Hash
    deriving (Show, Eq)

type GitOrphanList = [GitOrphan]

data GitObject = GitBlobObject   { objectHash :: Hash, objectSize :: Size }
               | GitCommitObject { objectHash :: Hash, objectSize :: Size, commitTree :: Maybe Hash, commitParents :: Maybe [Hash] }
               | GitTreeObject   { objectHash :: Hash, objectSize :: Size }
    deriving (Show, Eq)

simpleGitCommit :: Hash -> Size -> GitObject
simpleGitCommit h s = GitCommitObject h s Nothing Nothing

type GitObjectList = [GitObject]

type Modes = Integer
data GitTreeEntry = GitTreeEntry { treeMode :: Modes, treeObject :: GitObject, treeName :: FilePath }
    deriving (Show, Eq)
