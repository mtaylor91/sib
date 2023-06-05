{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Build
  ( buildImage
  ) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (catch, throw)
import Control.Monad (unless, when)
import qualified Data.Text as T
import System.Directory (createDirectory, getCurrentDirectory, withCurrentDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals

import Lib.Context
import Lib.Command
import Lib.Error
import Lib.File (writeFile)
import Lib.HTTP (downloadFile)
import Lib.Spec (Spec)
import Lib.State
import Lib.Steps (fromSpec, Step(..), Steps(..))


buildImage :: Spec -> IO ()
buildImage spec = do
  buildThreadId <- myThreadId
  let handleInterrupt = throwTo buildThreadId BuildInterrupted
      exitSignalSet = addSignal sigQUIT $ addSignal sigTERM emptySignalSet
  _ <- installHandler sigINT (Catch handleInterrupt) $ Just exitSignalSet
  cwd <- getCurrentDirectory
  state <- fromSpec spec >>=
    ( runSteps $ State
      { contextStack = []
      , directoryStack = []
      , startingDirectory = cwd
      , chrootDirectory = Nothing
      , failed = False
      }
    )
  when (failed state) $ throw BuildFailed


enterContext :: State -> Context -> IO State
enterContext state ctx@(Chroot dir) = if failed state then pure state else do
  putStrLn ""
  putStrLn $ "Entering chroot " ++ T.unpack dir
  pure $ pushContext (state { chrootDirectory = Just $ T.unpack dir }) ctx
enterContext state ctx@(Directory dir) = do
  putStrLn ""
  putStrLn $ "Entering directory " ++ T.unpack dir
  pure $ pushContext (pushDirectory state $ T.unpack dir) ctx
enterContext state ctx@(BracketCommands n enterCommands _) = do
  putStrLn ""
  putStrLn $ "Entering " ++ T.unpack n
  state' <- executeCommands state enterCommands
  pure $ pushContext state' ctx


leaveContext :: State -> Context -> IO State
leaveContext state ctx@(Chroot dir) = do
  putStrLn ""
  putStrLn $ "Leaving chroot " ++ T.unpack dir
  pure $ removeContext (state { chrootDirectory = Nothing }) ctx
leaveContext state ctx@(Directory dir) = do
  putStrLn ""
  putStrLn $ "Leaving directory " ++ T.unpack dir
  pure $ removeContext (removeDirectory state $ T.unpack dir) ctx
leaveContext state ctx@(BracketCommands n _ leaveCommands) = do
  putStrLn ""
  putStrLn $ "Leaving " ++ T.unpack n
  state' <- executeCommands state leaveCommands
  pure $ removeContext state' ctx


leaveRemainingContexts :: State -> IO State
leaveRemainingContexts state =
  case contextStack state of
    [] -> pure state
    (context:_) -> do
      state' <- leaveContext state context
      leaveRemainingContexts state'


runSteps :: State -> Steps -> IO State
runSteps state (Steps []) =
  leaveRemainingContexts state
runSteps state (Steps (step:steps)) =
  if failed state
    then case step of
      LeaveContext _ -> doRunStep
      _ -> skipRunStep
    else doRunStep
  where
    doRunStep :: IO State
    doRunStep = do
      state' <- catchFail state $ doRunStepWithDirectoryStack $ directoryStack state
      runSteps state' $ Steps steps
    doRunStepWithDirectoryStack :: [FilePath] -> IO State
    doRunStepWithDirectoryStack [] = runStep state step
    doRunStepWithDirectoryStack (dir:_) = catch
      (withCurrentDirectory dir $ runStep state step)
      (doRunRetryCreateDirectory dir)
    doRunRetryCreateDirectory :: FilePath -> IOError -> IO State
    doRunRetryCreateDirectory dir e = do
      unless (isDoesNotExistError e) $ ioError e
      createDirectory dir
      withCurrentDirectory dir $ runStep state step
    skipRunStep :: IO State
    skipRunStep = do
      runSteps state $ Steps steps


runStep :: State -> Step -> IO State
runStep state (RunCommand cmd) =
  runCommand state cmd
runStep state (RunCommands commands) =
  runCommands state commands
runStep state (WriteFile file content) =
  Lib.File.writeFile state file content
runStep state (DownloadFile file url) = do
  downloadFile state (T.unpack file) (T.unpack url)
runStep state (EnterContext ctx) =
  enterContext state ctx
runStep state (LeaveContext ctx) =
  leaveContext state ctx
