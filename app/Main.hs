module Main where

import Parser

import TccCore.TccParser
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

-- compileFile :: FilePath -> IO FilePath
-- compileFile path = do
--     handle <- openFile path ReadMode
--     contents <- hGetContents handle
--     astProgEither <- return $ prog <@> contents
--     case astProgEither of
--       Right (astProg, _) -> do
--           writeFile asmFile (tccGenerate astProg)
--           return True
--       Left err -> do
--                     mapM_ putStrLn err
--                     return False
--       where asmFile = dropExtension path ++ asmExt

-- getFiles args = filterM doesFileExist args

-- createObjFiles files = do
--     exitCode <- system ("gcc -c " ++ (intercalate " " files))
--     return (exitCode == ExitSuccess)


-- changeFilesExt newExt files = map changeExt files
--     where changeExt = ((flip addExtension) newExt) . dropExtension
--

removeFiles :: [FilePath] -> IO [()]
-- removeFiles = mapM (\f -> when (doesFileExist f) (removeFile f))
removeFiles = undefined

-- buildExec files = do
--     exitCode <- system ("gcc " ++ (intercalate " " files))
--     return (exitCode == ExitSuccess)
--
allTrue = and

execWithArgs :: [String] -> IO ()
execWithArgs args = do
                  files <- getFiles args
                  results <- mapM compileFile files
                  if (allTrue results)
                     then do
                         let asmFiles = changeFilesExt asmExt files
                         objSuccess <- createObjFiles asmFiles
                         removeFiles asmFiles
                         return 0
--                          else
--                              return (-1)
--                       --     when objSuccess $ do
--                       --         let objFiles = changeFilesExt objExt files
--                       --         buildSuccess <- buildExec objFiles
--                       --         removeFiles objFiles
--                       --         if buildSuccess
--                       --            then return 0
--                       --            else return (-1)


main :: IO ()
main = do
    args <- getArgs
    execWithArgs args
    return ()
