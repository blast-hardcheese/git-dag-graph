{-# LANGUAGE FlexibleContexts #-}
module GitParsers where

import Control.Applicative ((<*>),(<$>))
import Control.Monad.Identity
import Text.Parsec
import Data.Maybe (fromMaybe)

import Types

fromBaseN :: (Num a, Ord a, Read a) => a -> String -> a
fromBaseN base = foldl (\a x -> a * 8 + (read [x])) 0

fromOctal :: (Num a, Ord a, Read a) => String -> a
fromOctal = fromBaseN 8

octal :: Stream s m Char => ParsecT s u m Integer
octal = fromOctal <$> many1 octDigit

space1 :: Stream s m Char => Monad m => ParsecT s u m [Char]
space1 = many1 (char ' ')

dropLine :: Stream s m Char => ParsecT s u m [Char]
dropLine = anyChar `manyTill` (lookAhead endOfLine)

labelNum :: Stream s m Char => String -> ParsecT s u m Int
labelNum label = do { try (do _ <- string label; return ()); _ <- char ':'; _ <- many space; res <- many1 digit; return (read res) }

accGitObjects :: ParsecT String GitObjectStats Identity GitObjectStats
accGitObjects = do { res <- fields `endBy1` (char '\n'); getState }
  where store :: String -> (Int -> GitObjectStats -> GitObjectStats) -> ParsecT String GitObjectStats Identity GitObjectStats
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

parseGitObjects :: String -> Either ParseError GitObjectStats
parseGitObjects input = do
  let init = GitObjectStats 0 0 0 0 0 0 0 0
  runParser accGitObjects init "" input

accOrphans :: ParsecT String GitOrphanList Identity GitOrphanList
accOrphans = do { res <- fields `endBy` (char '\n'); modifyState reverse; getState }
  where accumulate :: String -> (Hash -> GitOrphan) -> ParsecT String GitOrphanList Identity GitOrphanList
        accumulate kind f = do { try (do _ <- string "unreachable "; _ <- string kind; return ()); _ <- many space; hash <- parseHash; modifyState ((f hash) :); getState}
        fields = (choice [
                          accumulate "blob" OrphanBlob,
                          accumulate "commit" OrphanCommit
                         ])

parseHash :: Stream s m Char => ParsecT s u m [Char]
parseHash = many1 hexDigit

parseGitOrphanList :: String -> Either ParseError GitOrphanList
parseGitOrphanList input = do
  let init = []
  runParser accOrphans init "" input

parseHashKindSize :: Monad m => String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject
parseHashKindSize kind f = try (do
  hash <- parseHash
  _ <- space1
  _ <- string kind
  _ <- space1
  size <- (read <$> many1 digit) <|> ((\x -> (-1)) <$> char '-')
  return (f hash size))

parseKindHashSize :: Monad m => String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject
parseKindHashSize kind f = do
  _ <- try (string kind)
  _ <- space1
  hash <- parseHash
  _ <- space1
  size <- (read <$> many1 digit) <|> ((\x -> (-1)) <$> char '-')
  return (f hash size)

objectDesc :: Monad m => (String -> (Hash -> Size -> GitObject) -> ParsecT String u m GitObject) -> ParsecT String u m GitObject
objectDesc f = (choice [
                f "blob" GitBlobObject,
                f "commit" simpleGitCommit,
                f "tree" GitTreeObject
              ])

accObjects :: ParsecT String [GitObject] Identity [GitObject]
accObjects = do { _ <- (do o <- (objectDesc parseHashKindSize); modifyState (o:); return ()) `endBy` (char '\n'); modifyState reverse; getState }

parseGitObjectList :: String -> Either ParseError [GitObject]
parseGitObjectList input = do
  let init = []
  runParser accObjects init "" input

fileName :: Stream s m Char => ParsecT s u m String
fileName = many1 $ choice (alphaNum : (char <$> "-_ ."))

parseTreeLine :: ParsecT String u Identity GitTreeEntry
parseTreeLine = do
                  mode <- octal
                  _ <- space1
                  desc <- (objectDesc parseKindHashSize)
                  _ <- char '\t'
                  name <- fileName
                  return $ GitTreeEntry mode desc name

parseTree :: String -> Either ParseError [GitTreeEntry]
parseTree x = parse (parseTreeLine `endBy` (char '\n')) "" x

catCommitLines :: ParsecT String GitObject Identity GitObject
catCommitLines = do
  let options = (choice [
               f "tree" parseHash (\v s -> s { commitTree = Just v })
             , f "parent" parseHash (\v s -> s { commitParents = (commitParents s) ++ [v] })
             , f "author" dropLine (\v s -> s)
             , f "committer" dropLine (\v s -> s)
             ])
  _ <- options `endBy1` (char '\n')
  getState
  where f :: Stream s m Char => String -> ParsecT s u m v -> (v -> u -> u) -> ParsecT s u m u
        f label parser update = do { _ <- try (string label); _ <- space1; v <- parser; modifyState (update v); getState }

parseCatCommit :: GitObject -> String -> Either ParseError GitObject
parseCatCommit init x = runParser catCommitLines init "" x
