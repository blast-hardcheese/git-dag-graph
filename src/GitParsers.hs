{-# LANGUAGE FlexibleContexts #-}
module GitParsers where

import Control.Applicative ((<*>),(<$>))
import Control.Monad.Identity
import Text.Parsec

import Types

fromBaseN :: (Num a, Ord a, Read a) => a -> String -> a
fromBaseN base = foldl (\a x -> a * 8 + (read [x])) 0

fromOctal :: (Num a, Ord a, Read a) => String -> a
fromOctal = fromBaseN 8

octal :: Stream s m Char => ParsecT s u m Integer
octal = fromOctal <$> many1 octDigit

labelNum :: Stream s m Char => String -> ParsecT s u m Int
labelNum label = do { try (do _ <- string label; return ()); _ <- char ':'; _ <- many space; res <- many1 digit; return (read res) }

accGitObjects :: ParsecT String GitObjects Identity GitObjects
accGitObjects = do { res <- fields `endBy1` (char '\n'); getState }
  where store :: String -> (Int -> GitObjects -> GitObjects) -> ParsecT String GitObjects Identity GitObjects
        store label f = do { val <- (labelNum label); modifyState (f val); getState }
        fields = (choice [
                          store "size-pack"      (\v s -> s { size_pack = v }),
                          store "size-garbage"   (\v s -> s { size_garbage = v }),
                          store "size"           (\v s -> s { size = v }),
                          store "prune-packable" (\v s -> s { prune_packable = v }),
                          store "packs"          (\v s -> s { packs = v }),
                          store "in-pack"        (\v s -> s { in_pack = v }),
                          store "garbage"        (\v s -> s { garbage = v }),
                          store "count"          (\v s -> s { Types.count = v })
                         ])

parseGitObjects :: String -> Either String GitObjects
parseGitObjects input = do
  let init = GitObjects 0 0 0 0 0 0 0 0
  case (runParser accGitObjects init "" input) of
    Left err     -> Left $ show err
    Right result -> Right result

accOrphans :: ParsecT String GitOrphanList Identity GitOrphanList
accOrphans = do { res <- fields `endBy` (char '\n'); modifyState reverse; getState }
  where accumulate :: String -> (Hash -> GitOrphan) -> ParsecT String GitOrphanList Identity GitOrphanList
        accumulate kind f = do { try (do _ <- string "unreachable "; _ <- string kind; return ()); _ <- many space; hash <- many1 hexDigit; modifyState ((f hash) :); getState}
        fields = (choice [
                          accumulate "blob" OrphanBlob,
                          accumulate "commit" OrphanCommit
                         ])

parseGitOrphanList :: String -> Either String GitOrphanList
parseGitOrphanList input = do
  let init = []
  case (runParser accOrphans init "" input) of
    Left err     -> Left $ show err
    Right result -> Right result

objectDesc :: Monad m => ParsecT String u m GitObject
objectDesc = (choice [
                parseLine "blob" GitBlobObject,
                parseLine "commit" GitCommitObject,
                parseLine "tree" GitTreeObject
              ])
  where parseLine :: Monad m => String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject
        parseLine kind f = try (do
                                   hash <- many1 hexDigit
                                   _ <- char ' '
                                   _ <- string kind
                                   _ <- char ' '
                                   size <- read <$> many1 digit
                                   return (f hash size))

accObjects :: ParsecT String GitObjectList Identity GitObjectList
accObjects = do { _ <- (do o <- objectDesc; modifyState (o:); return ()) `endBy` (char '\n'); modifyState reverse; getState }

parseGitObjectList :: String -> Either String GitObjectList
parseGitObjectList input = do
  let init = []
  case (runParser accObjects init "" input) of
    Left err    -> Left $ show err
    Right result -> Right result
