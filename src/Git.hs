module Git where

import Types
import qualified GitParsers

import Control.Applicative

import Data.Maybe (isJust, listToMaybe)

import System.Process
import GHC.IO.Handle

import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import Numeric (readHex)

readMaybeHex :: String -> Maybe Int
readMaybeHex = (fst <$>) . listToMaybe . readHex

runStdOut :: String -> [String] -> FilePath -> IO String
runStdOut cmd args fp = do
  (_, Just hout, _, _) <-
      createProcess (proc cmd args) { cwd = Just fp, std_out = CreatePipe }

  hGetContents hout

countObjects :: FilePath -> IO (Either String GitObjects)
countObjects fp = GitParsers.parseGitObjects <$> runStdOut "git" ["count-objects", "-v"] fp

findOrphans :: FilePath -> IO (Either String GitOrphanList)
findOrphans fp = GitParsers.parseGitOrphanList <$> runStdOut "git" ["fsck", "--unreachable"] fp

getAllObjectHashes :: FilePath -> IO [String]
getAllObjectHashes fp = do
  let objectDir = fp </> ".git/objects"
  files <- getDirectoryContents objectDir
  let objectDirs = filter (\x -> length x == 2 && isJust (readMaybeHex x)) files
  (concat <$>) . sequence $ (getDirectoryContentsPrependingPath objectDir) <$> objectDirs
  where filterDots :: [String] -> [String]
        filterDots = filter (\x -> x /= "." && x /= "..")
        getDirectoryContentsPrependingPath :: FilePath -> FilePath -> IO [FilePath]
        getDirectoryContentsPrependingPath objectDir x = ((x ++) <$>) <$> filterDots <$> (getDirectoryContents $ objectDir </> x)
