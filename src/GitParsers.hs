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

space1 :: Stream s m Char => Monad m => ParsecT s u m [Char]
space1 = many1 (char ' ')

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

parseHashKindSize :: Monad m => String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject
parseHashKindSize kind f = try (do
  hash <- many1 hexDigit
  _ <- char ' '
  _ <- string kind
  _ <- char ' '
  size <- read <$> many1 digit
  return (f hash size))

parseKindHashSize :: Monad m => String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject
parseKindHashSize kind f = try (do
  _ <- string kind
  _ <- space1
  hash <- many1 hexDigit
  _ <- space1
  size <- read <$> many1 digit
  return (f hash size))

objectDesc :: Monad m => (String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject) -> ParsecT String u m GitObject
objectDesc f = (choice [
                f "blob" GitBlobObject,
                f "commit" GitCommitObject,
                f "tree" GitTreeObject
              ])

accObjects :: ParsecT String GitObjectList Identity GitObjectList
accObjects = do { _ <- (do o <- (objectDesc parseHashKindSize); modifyState (o:); return ()) `endBy` (char '\n'); modifyState reverse; getState }

parseGitObjectList :: String -> Either String GitObjectList
parseGitObjectList input = do
  let init = []
  case (runParser accObjects init "" input) of
    Left err    -> Left $ show err
    Right result -> Right result

fileName :: Stream s m Char => ParsecT s u m String
fileName = many1 $ choice (alphaNum : (fmap char "_ ."))

parseTreeLine :: ParsecT String u Identity GitTreeEntry
parseTreeLine = do
                  mode <- octal
                  _ <- space1
                  desc <- (objectDesc parseKindHashSize)
                  _ <- char '\t'
                  name <- fileName
                  return $ GitTreeEntry mode desc name

parseTree :: String -> Either String [GitTreeEntry]
parseTree x = case (parse (parseTreeLine `endBy` (char '\n')) "" x) of
  Left err     -> Left $ show err
  Right result -> Right result
