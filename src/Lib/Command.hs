{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Command
  ( executeCommand
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throw)
import qualified Data.Text as T
import System.IO (Handle, hClose, hGetLine, hIsEOF)
import System.Process
import System.Exit

import Lib.Context (Stack, chroot, directory, indent)
import Lib.Error (BuildException(..))


executeCommand :: Stack -> [T.Text] -> IO ()
executeCommand stack command =
  executeCommandWithChroot
  where
    executeCommandWithChroot :: IO ()
    executeCommandWithChroot =
      case chroot stack of
        Just dir -> do
          let cmdline = "chroot" : dir : command
          putStrLn $ indent stack <> "Executing: " ++ show cmdline
          executeCommandProcess Nothing cmdline
        Nothing -> do
          let dir = directory stack
          putStrLn $ indent stack <> "Executing: " ++ show command
          executeCommandProcess (Just $ T.unpack dir) command
    executeCommandProcess :: Maybe FilePath -> [T.Text] -> IO ()
    executeCommandProcess _ [] = pure ()
    executeCommandProcess maybeDir (cmd:args) = do
      (readIn, writeIn) <- createPipe
      (readOut, writeOut) <- createPipe
      let process = (proc (T.unpack cmd) (map T.unpack args))
            { cwd = maybeDir
            , std_in = UseHandle readIn
            , std_out = UseHandle writeOut
            , std_err = UseHandle writeOut
            }
      outputSync <- newEmptyMVar
      (_, _, _, ph) <- createProcess process
      hClose writeIn
      _ <- forkIO $ writeCommandOutput readOut >> putMVar outputSync ()
      exitCode <- waitForProcess ph
      takeMVar outputSync
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> do
          let cmdString = T.intercalate " " (cmd:args)
              errString = "Command failed: " ++ T.unpack cmdString ++
                          " (exit code: " ++ show code ++ ")"
          throw $ BuildCommandFailed errString
    writeCommandOutput :: Handle -> IO ()
    writeCommandOutput handle = do
      iseof <- hIsEOF handle
      if iseof then pure () else do
        line <- hGetLine handle
        putStrLn $ indent stack ++ line
        writeCommandOutput handle
