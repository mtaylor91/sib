{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Build
  ( buildImage
  ) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (throw)
import Control.Monad (when)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.Posix.Signals

import Lib.Command
import Lib.Error
import Lib.File (writeFile)
import Lib.HTTP (downloadFile)
import Lib.Spec
import Lib.State


buildImage :: Spec -> IO ()
buildImage spec = do
  buildThreadId <- myThreadId
  let handleInterrupt = throwTo buildThreadId BuildInterrupted
      exitSignalSet = addSignal sigQUIT $ addSignal sigTERM emptySignalSet
  _ <- installHandler sigINT (Catch handleInterrupt) $ Just exitSignalSet
  cwd <- getCurrentDirectory
  state <- runSteps (steps spec) $ State
    { contextStack = []
    , startingDirectory = cwd
    , failed = False
    }
  when (failed state) $ throw BuildFailed


enterContext :: State -> T.Text -> Context -> IO State
enterContext state name ctx@(Chroot dir) = do
  putStrLn $ "Entering chroot " ++ T.unpack dir
  pure $ pushContext state name ctx
enterContext state name ctx@(Directory dir) = do
  putStrLn $ "Entering directory " ++ T.unpack dir
  pure $ pushContext state name ctx
enterContext state name ctx@(BracketCommands enterCommands _) = do
  putStrLn $ "Entering " ++ T.unpack name
  state' <- executeCommands state enterCommands
  pure $ pushContext state' name ctx


leaveContext :: State -> T.Text -> IO State
leaveContext state n =
  case popContext state of
    (state', Just (n', ctx)) | n == n' ->
      leaveContext' state' ctx
    (_, Nothing) ->
      error "leaveContext: context stack mismatch (empty)"
    (_, Just (n', _)) ->
      error $ "leaveContext: context stack mismatch (" ++ T.unpack n' ++ " != " ++ T.unpack n ++ ")"
  where
    leaveContext' :: State -> Context -> IO State
    leaveContext' state (Chroot dir) = do
      putStrLn $ "Leaving chroot " ++ T.unpack dir
      pure state
    leaveContext' state (Directory dir) = do
      putStrLn $ "Leaving directory " ++ T.unpack dir
      pure state
    leaveContext' state (BracketCommands _ leaveCommands) = do
      putStrLn $ "Leaving " ++ T.unpack n
      executeCommands state leaveCommands


leaveRemainingContexts :: State -> IO State
leaveRemainingContexts state =
  case contextStack state of
    [] -> pure state
    ((name, _):_) -> do
      putStrLn ""
      state' <- leaveContext state name
      leaveRemainingContexts state'


runSteps :: [Step] -> State -> IO State
runSteps [] state =
  leaveRemainingContexts state
runSteps (step:steps) state =
  if failed state
    then case step of
      LeaveContext _ -> doRunStep
      _ -> skipRunStep
    else doRunStep
  where
    doRunStep :: IO State
    doRunStep = do
      state' <- catchFail state $ runStep state step
      runSteps steps state'
    skipRunStep :: IO State
    skipRunStep = do
      runSteps steps state


runStep :: State -> Step -> IO State
runStep state step = putStrLn "" >> runStepDispatch state step


runStepDispatch :: State -> Step -> IO State
runStepDispatch state (DownloadFile file sha512 url) =
  downloadFile state (T.unpack file) (T.unpack sha512) (T.unpack url)
runStepDispatch state (EnterContext name ctx) =
  enterContext state name ctx
runStepDispatch state (LeaveContext name) = do
  leaveContext state name
runStepDispatch state (RunCommands commands) =
  runCommands state commands
runStepDispatch state (WriteFile file content) =
  Lib.File.writeFile state file content
