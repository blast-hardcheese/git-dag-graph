module Git where

import Types

import Control.Applicative

import System.Process
import GHC.IO.Handle

parseString :: String -> Maybe GitObjects
parseString _ = Just $ GitObjects 0 0 0 0 0 0 0 0

countObjects :: FilePath -> IO (Maybe GitObjects)
countObjects fp = do
  (_, Just hout, _, _) <-
      createProcess (proc "git" ["count-objects", "-v"]) { cwd = Just fp, std_out = CreatePipe }

  (\x -> (parseString x) >> Nothing) <$> (hGetContents hout)
