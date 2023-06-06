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
import Lib.Validate


buildImage :: Spec -> IO ()
buildImage spec = do
  buildThreadId <- myThreadId
  let handleInterrupt = throwTo buildThreadId BuildInterrupted
      exitSignalSet = addSignal sigQUIT $ addSignal sigTERM emptySignalSet
  _ <- installHandler sigINT (Catch handleInterrupt) $ Just exitSignalSet
  cwd <- getCurrentDirectory
  let state = State
        { spec = spec
        , contextStack = []
        , startingDirectory = cwd
        , stepsRemaining = steps spec
        , failed = False
        }
  validateState state
  state' <- runState state
  when (failed state') $ throw BuildFailed
  putStrLn ""
  putStrLn "Build finished."


dispatchStep :: State -> Step -> IO State
dispatchStep state (DownloadFile file sha512 url) =
  downloadFile state (T.unpack file) (T.unpack sha512) (T.unpack url)
dispatchStep state (EnterContext name ctx) =
  enterContext state name ctx
dispatchStep state (LeaveContext name) = do
  leaveContext state name
dispatchStep state (RunCommands commands) =
  runCommands state commands
dispatchStep state (WriteFile file content) =
  Lib.File.writeFile state file content


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


runState :: State -> IO State
runState state =
  case stepsRemaining state of
    [] -> leaveRemainingContexts state
    step:stepsRemaining' ->
      if failed state
        then case step of
          LeaveContext _ ->
            doRunStep step stepsRemaining'
          _ -> skipRunStep stepsRemaining'
        else doRunStep step stepsRemaining'
  where
    doRunStep :: Step -> [Step] -> IO State
    doRunStep step stepsRemaining' = do
      state' <- catchFail state $ runStep state step
      runState $ state' { stepsRemaining = stepsRemaining' }
    skipRunStep :: [Step] -> IO State
    skipRunStep stepsRemaining' =
      runState $ state { stepsRemaining = stepsRemaining' }


runStep :: State -> Step -> IO State
runStep state step = putStrLn "" >> dispatchStep state step
