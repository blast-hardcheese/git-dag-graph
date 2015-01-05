module Git where

import Types
import qualified GitParsers

import Control.Applicative

import System.Process
import GHC.IO.Handle

runStdOut :: String -> [String] -> FilePath -> IO String
runStdOut cmd args fp = do
  (_, Just hout, _, _) <-
      createProcess (proc cmd args) { cwd = Just fp, std_out = CreatePipe }

  hGetContents hout

countObjects :: FilePath -> IO (Either String GitObjects)
countObjects fp = GitParsers.parseGitObjects <$> runStdOut "git" ["count-objects", "-v"] fp

findOrphans :: FilePath -> IO (Either String GitOrphanList)
findOrphans fp = GitParsers.parseGitOrphanList <$> runStdOut "git" ["fsck", "--unreachable"] fp
