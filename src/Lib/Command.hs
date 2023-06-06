{-# LANGUAGE OverloadedStrings #-}
module Lib.Command
  ( Lib.Command.executeCommand
  , Lib.Command.executeCommands
  , Lib.Command.runCommands
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad
import qualified Data.Text as T
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
          let cmdline = "chroot" : T.pack chroot : command
          putStrLn $ "  Executing: " ++ show cmdline
          executeCommandProcess Nothing cmdline
        Nothing -> do
          case contextDirectory state of
            Just dir -> do
              putStrLn $ "  Executing: " ++ show command
              executeCommandProcess (Just dir) command
            Nothing -> do
              putStrLn $ "  Executing: " ++ show command
              executeCommandProcess Nothing command
    executeCommandProcess :: Maybe FilePath -> [T.Text] -> IO State
    executeCommandProcess _ [] = pure state
    executeCommandProcess maybeDir (cmd:args) = do
      (readFD, writeFD) <- createPipe
      let process = (proc (T.unpack cmd) (map T.unpack args))
            { cwd = maybeDir
            , delegate_ctlc = True
            , std_in = NoStream
            , std_out = UseHandle writeFD
            , std_err = UseHandle writeFD
            }
      outputSync <- newEmptyMVar
      (_, _, _, ph) <- createProcess process
      _ <- forkIO $ writeCommandOutput readFD >> putMVar outputSync ()
      exitCode <- waitForProcess ph
      takeMVar outputSync
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
