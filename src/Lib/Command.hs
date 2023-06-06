{-# LANGUAGE OverloadedStrings #-}
module Lib.Command
  ( Lib.Command.executeCommand
  , Lib.Command.executeCommands
  , Lib.Command.runCommands
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.IO (Handle, hGetLine, hIsEOF)
import System.Process
import System.Exit

import Lib.Spec (CommandOrCommands(..))
import Lib.State


executeCommand :: State -> [T.Text] -> IO State
executeCommand state command =
  executeCommandWithChroot
  where
    executeCommandWithChroot :: IO State
    executeCommandWithChroot =
      case chrootDirectory state of
        Just chroot -> do
          dir <- getCurrentDirectory
          let cmdline = "chroot" : T.pack chroot : command
          putStrLn $ "  Executing: " ++ show cmdline
          putStrLn $ "  In directory: " ++ show dir
          executeCommandProcess cmdline
        Nothing -> do
          dir <- getCurrentDirectory
          putStrLn $ "  Executing: " ++ show command
          putStrLn $ "  In directory: " ++ show dir
          executeCommandProcess command
    executeCommandProcess :: [T.Text] -> IO State
    executeCommandProcess [] = pure state
    executeCommandProcess (cmd:args) = do
      (readFD, writeFD) <- createPipe
      let process = (proc (T.unpack cmd) (map T.unpack args))
            { delegate_ctlc = True
            , std_in = NoStream
            , std_out = UseHandle writeFD
            , std_err = UseHandle writeFD
            }
      (_, _, _, ph) <- createProcess process
      outputThreadId <- forkIO $ writeCommandOutput readFD
      exitCode <- waitForProcess ph
      killThread outputThreadId
      case exitCode of
        ExitSuccess -> pure state
        ExitFailure code -> do
          putStrLn $ "  Command failed with exit code: " ++ show code
          pure $ state { failed = True }
    writeCommandOutput :: Handle -> IO ()
    writeCommandOutput handle = do
      iseof <- hIsEOF handle
      if iseof then pure () else do
        line <- hGetLine handle
        putStrLn $ "    " ++ line
        writeCommandOutput handle


executeCommands :: State -> CommandOrCommands -> IO State
executeCommands state (Command cmd) = executeCommand state cmd
executeCommands state (Commands cmds) = foldM executeCommand state cmds


runCommands :: State -> CommandOrCommands -> IO State
runCommands state commands = do
  putStrLn "Running commands:"
  executeCommands state commands
