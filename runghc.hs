--  Copyright (C) 2011 Michael Zuser mikezuser@gmail.com
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.Console.CmdArgs hiding (help)
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.IO
import System.Process
import System.Time

help = intercalate "\n"
    [ "This is a replacement for runghc which keeps compiled files"
    , "for fast runtimes when nothing has changed"
    , ""
    , "Usage: runghc [options] [ghc-options] file [file-arguments]"
    , ""
    , "Options:"
    , " -! --compile-only  Do not run the script, just compile it"
    , "    --exe-dir=DIR   Directory for the executable"
    , "                        default=<scr-dir>/.compile" 
    , "    --out-dir=DIR   Directory for the intermediate files"
    , "                        default=<exe-dir>/<basename>-intermediate-files"
    , " -? --help          Display this help message"
    , ""
    ]

data OptDesc = OptDesc
    { fCmpOnly :: Bool
    , fExeDir :: Maybe FilePath
    , fOutDir :: Maybe FilePath
    } deriving (Show, Data, Typeable)

optDesc = OptDesc
    { fCmpOnly = False &= explicit &= name "compile-only" &= name "!"
    , fExeDir = def &= explicit &= name "exe-dir"
    , fOutDir = def &= explicit &= name "out-dir"
    } &= versionArg [ignore]

main :: IO ()
main = do
    (caOpts, args) <- getOpts <$> getArgs
    when (isJust $ cmdArgsHelp caOpts) $
        putStr help >> exitSuccess

    let opts = cmdArgsValue caOpts
    let (ghcOpts, fileAndArgs) = splitGHCOpts args
    when (null fileAndArgs) $
        putStr help >> exitSuccess

    let (file:fileArgs) = fileAndArgs
    let exe = getExe file opts
    let outDir = getOutDir file opts

    whenM (shouldCompile file exe outDir) $ 
        compile ghcOpts file exe outDir
    unless (fCmpOnly opts) $ 
        exitWith =<< rawSystem exe fileArgs
  where
    splitGHCOpts = break ((== ".hs") . takeExtension)

getOpts :: [String] -> (CmdArgs OptDesc, [String])
getOpts args = (opts, drop (length nopts) args) 
  where
    (Right opts : nopts) = reverse . takeWhile isRight . map processArgs . inits $ args 
    isRight e = case e of Right _ -> True ; Left _ -> False
    processArgs = process (cmdArgsMode optDesc)

shouldCompile :: FilePath -> FilePath -> FilePath -> IO Bool
shouldCompile file exe outDir = ifteM (doesFileExist exe) newerSrcs (return True)
  where
    modTime = getModificationTime
    newerSrcs = do
        exeTime <- modTime exe
        let isNewer s = ifteM (doesFileExist s) ((exeTime <) <$> modTime s) (return True)
        (Nothing /=) <$> (findM isNewer =<< getSrcs file outDir)

getSrcs :: FilePath -> FilePath -> IO [FilePath]
getSrcs file outDir = (file :) . thereIsNoMain . concatMap hss <$> getFiles outDir
  where
    thereIsNoMain = filter $ (/= "Main") . takeBaseName
    inSrcDir src = takeDirectory file </> makeRelative outDir src
    asHs = flip replaceExtension "hs"
    hss f = case takeExtension f of
        ".hi" -> [inSrcDir . asHs $ f]
        _     -> []

compile :: [String] -> FilePath -> FilePath -> FilePath -> IO ()
compile ghcOpts file exe outDir = do
    createDirectoryIfMissing True $ takeDirectory exe
    runGHC fullOpts
  where 
    fullOpts = ["--make","-o",exe,"-outputdir",outDir] ++ ghcOpts ++ [file]
        
runGHC :: [String] -> IO ()
runGHC opts = do
    Just ghc <- findExecutable "ghc"
    (ec, _, err) <- readProcessWithExitCode ghc opts ""
    when (ec /= ExitSuccess) $ do
        hPutStr stderr err
        exitWith ec

getExeDir, getOutDir, getExe :: FilePath -> OptDesc -> FilePath
getExeDir file opts = fromMaybe def $ fExeDir opts
  where def = takeDirectory file </> ".compiled"
getOutDir file opts = fromMaybe def $ fOutDir opts
  where def = getExeDir file opts </> takeBaseName file ++ "-intermediate-files"
getExe file opts = getExeDir file opts </> takeBaseName file

-- Utility
ifteM :: Monad m => m Bool -> m a -> m a -> m a
ifteM b x y = b >>= \b -> if b then x else y

whenM :: Monad m => m Bool -> m () -> m ()
whenM b x = ifteM b x $ return ()

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM pred = go
  where
    go []     = return Nothing
    go (a:as) = ifteM (pred a) (return (Just a)) (go as)

getFiles :: FilePath -> IO [FilePath]
getFiles d
    =   return . concat
    =<< mapM expand
    =<< return . map (d </>) . filter notDots
    =<< getDirectoryContents d
  where 
    notDots f = all (/= f) [".",".."]
    expand f = ifteM (doesDirectoryExist f) (getFiles f) (return [f])
  
