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

