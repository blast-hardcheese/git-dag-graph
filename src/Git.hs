module Git where

import Control.Applicative

import System.Process
import GHC.IO.Handle

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

parseString :: String -> Maybe GitObjects
parseString _ = Just $ GitObjects 0 0 0 0 0 0 0 0

countObjects :: FilePath -> IO (Maybe GitObjects)
countObjects fp = do
  (_, Just hout, _, _) <-
      createProcess (proc "git" ["count-objects", "-v"]) { cwd = Just fp, std_out = CreatePipe }

  (\x -> (parseString x) >> Nothing) <$> (hGetContents hout)
