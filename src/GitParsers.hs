{-# LANGUAGE FlexibleContexts #-}
module GitParsers where

import Control.Monad.Identity

import Types

import Text.Parsec

labelNum :: Stream s m Char => String -> ParsecT s u m Int
labelNum label = do { try (do _ <- string label; return ()); _ <- char ':'; _ <- many space; res <- many1 digit; return (read res) }

accGitObjects :: ParsecT String GitObjects Identity GitObjects
accGitObjects = do { res <- fields `sepBy1` (char '\n'); getState }
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
