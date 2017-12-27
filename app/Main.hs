module Main where

import TccCore.Lexer
import TccCore.Parser
import TccCore.CodeGen

import System.Environment
import System.FilePath
import System.Directory (doesFileExist, removeFile)
import System.IO
import System.Process
import System.Exit

import Data.List (intercalate)

import Control.Monad (mapM, forM, filterM, when, unless)

asmExt = ".s"
objExt = ".o"

compileFile :: FilePath -> IO Bool
compileFile path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let tokens = tccLex contents
    case tccParse tokens of
      Right prog -> do
          writeFile asmFile (tccGenerate prog)
          return True
      Left errs -> do
          putStrLn $ unlines errs
          return False
      where asmFile = dropExtension path ++ asmExt

getFiles args = filterM doesFileExist args

createObjFiles files = do
    exitCode <- system ("gcc -c " ++ (intercalate " " files))
    return (exitCode == ExitSuccess)


changeFilesExt newExt files = map changeExt files
    where changeExt = ((flip addExtension) newExt) . dropExtension

removeFiles = mapM removeFile

buildExec files = do
    exitCode <- system ("gcc " ++ (intercalate " " files))
    return (exitCode == ExitSuccess)


main :: IO ()
main = do
    args <- getArgs
    files <- getFiles args
    results <- mapM compileFile files
    when (all id results) $ do
        let asmFiles = changeFilesExt asmExt files
        objSuccess <- createObjFiles asmFiles
        removeFiles asmFiles
        when objSuccess $ do
            let objFiles = changeFilesExt objExt files
            buildSuccess <- buildExec objFiles
            removeFiles objFiles
            unless buildSuccess $ putStrLn "Building failed. You are responsible for this!"
