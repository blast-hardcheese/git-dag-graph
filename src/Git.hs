module Git where

import Debug.Trace

import Types
import GitParsers (parseGitObjects, parseGitOrphanList, parseGitObjectList, parseTree, parseCatCommit)

import Control.Applicative

import Data.Either
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

runStdOutWithIn :: String -> [String] -> FilePath -> String -> IO String
runStdOutWithIn cmd args fp stdin = do
  (Just hin, Just hout, _, _) <-
      createProcess (proc cmd args) { cwd = Just fp, std_out = CreatePipe, std_in = CreatePipe }

  hPutStr hin stdin
  hClose hin
  hGetContents hout

countObjects :: FilePath -> IO (Either String GitObjects)
countObjects fp = parseGitObjects <$> runStdOut "git" ["count-objects", "-v"] fp

findOrphans :: FilePath -> IO (Either String GitOrphanList)
findOrphans fp = parseGitOrphanList <$> runStdOut "git" ["fsck", "--unreachable"] fp

getAllObjectHashes :: FilePath -> IO [FilePath]
getAllObjectHashes fp = do
  let objectDir = fp </> ".git/objects"
  files <- getDirectoryContents objectDir
  let objectDirs = filter (\x -> length x == 2 && isJust (readMaybeHex x)) files
  (concat <$>) . sequence $ (getDirectoryContentsPrependingPath objectDir) <$> objectDirs
  where filterDots :: [String] -> [String]
        filterDots = filter (\x -> x /= "." && x /= "..")
        getDirectoryContentsPrependingPath :: FilePath -> FilePath -> IO [FilePath]
        getDirectoryContentsPrependingPath objectDir x = ((x ++) <$>) <$> filterDots <$> (getDirectoryContents $ objectDir </> x)

objectHashesToObjects :: FilePath -> [FilePath] -> IO (Either String GitObjectList)
objectHashesToObjects fp objects = parseGitObjectList <$> runStdOutWithIn "git" ["cat-file", "--batch-check"] fp (unlines objects)

lsTree :: FilePath -> Hash -> IO (Either String [GitTreeEntry])
lsTree fp hash = parseTree <$> runStdOut "git" ["ls-tree", "-l", hash] fp

-- Given a list of gitObjects, extract treePairs and orphans
extractTrees :: FilePath -> [GitObject] -> IO ([(GitObject, [GitTreeEntry])], [GitObject])
extractTrees fp objects = do
  let trees = filter (\x -> case x of { (GitTreeObject h s) -> True; _ -> False }) objects :: [GitObject]

  let handle :: GitObject -> (Either String [GitTreeEntry]) -> (GitObject, [GitTreeEntry])
      handle o (Left err) = trace ("ERROR! " ++ err) (o, [])
      handle o (Right xs) = (o, xs)

  treePairs <- sequence $ map (\o -> (handle o) <$> (lsTree fp (objectHash o))) trees

  let removeClaimed :: [GitObject] -> [(GitObject, [GitTreeEntry])] -> [GitObject]
      removeClaimed xs pairs = filter ((flip notElem (concat $ claimedHashes <$> pairs)) . objectHash) xs
        where claimedHashes (o, xs) = objectHash <$> treeObject <$> xs

  let orphans = removeClaimed objects treePairs

  return (treePairs, orphans)

-- Given a list of gitObjects, extract commitPairs and orphans
extractCommits :: FilePath -> [GitObject] -> IO ([(GitObject, GitObject)], [GitObject])
extractCommits fp objects = do
  return ([], [])
