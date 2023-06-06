{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Command
  ( Lib.Command.executeCommand
  , Lib.Command.executeCommands
  , Lib.Command.runCommands
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throw)
import Control.Monad
import qualified Data.Text as T
import System.IO (Handle, hClose, hGetLine, hIsEOF)
import System.Process
import System.Exit

import Lib.Error (BuildException(..))
import Lib.Spec (CommandOrCommands(..), CommandOrCommandsStep(..))
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
      (readIn, writeIn) <- createPipe
      (readOut, writeOut) <- createPipe
      let process = (proc (T.unpack cmd) (map T.unpack args))
            { cwd = maybeDir
            , delegate_ctlc = True
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
        ExitSuccess -> pure state
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
        putStrLn $ "    " ++ line
        writeCommandOutput handle


executeCommands :: State -> CommandOrCommands -> IO State
executeCommands state (Command cmd) = executeCommand state cmd
executeCommands state (Commands cmds) = foldM executeCommand state cmds


runCommands :: State -> CommandOrCommandsStep -> IO State
runCommands state commandOrCommands = do
  putStrLn "Running commands:"
  case commandOrCommands of
    CommandStep cmd -> executeCommands state cmd
    CommandsStep commands -> executeCommands state commands
