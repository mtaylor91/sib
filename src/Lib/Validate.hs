{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Validate
  ( validateState
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import Network.HTTP.Client

import Lib.Spec (Context(..), Step(..))
import Lib.State


validateState :: State -> IO ()
validateState state = do
  case stepsRemaining state of
    [] -> pure ()
    (step:stepsRemaining') -> do
      validateStep state step stepsRemaining'
  where
    validateDownloadFile :: State -> FilePath -> String -> [Step] -> IO ()
    validateDownloadFile state file url stepsRemaining' = do
      validateFile state file
      validateUrl url
      validateState state { stepsRemaining = stepsRemaining' }
    validateEnterContext :: State -> T.Text -> Context -> [Step] -> IO ()
    validateEnterContext state name ctx stepsRemaining' = do
      let state' = pushContext state name ctx
      validateState state' { stepsRemaining = stepsRemaining' }
    validateLeaveContext :: State -> T.Text -> [Step] -> IO ()
    validateLeaveContext state name stepsRemaining' = do
      let (state', maybePopped) = popContext state
      case maybePopped of
        Nothing ->
          fail $ "Leave context " ++ T.unpack name ++ " failed (no context to leave)"
        Just (name', _) ->
          if name /= name'
            then fail $ "Leave context " ++ T.unpack name ++ " failed (expected " ++
              T.unpack name' ++ ", got " ++ T.unpack name ++ ")"
            else validateState state' { stepsRemaining = stepsRemaining' }
    validateWriteFile :: State -> FilePath -> [Step] -> IO ()
    validateWriteFile state file stepsRemaining' = do
      validateFile state file
      validateState state { stepsRemaining = stepsRemaining' }
    validateStep :: State -> Step -> [Step] -> IO ()
    validateStep state (DownloadFile file _ url) stepsRemaining' =
      validateDownloadFile state (T.unpack file) (T.unpack url) stepsRemaining'
    validateStep state (EnterContext name ctx) stepsRemaining' = do
      validateEnterContext state name ctx stepsRemaining'
    validateStep state (LeaveContext name) stepsRemaining' = do
      validateLeaveContext state name stepsRemaining'
    validateStep state (RunCommands _) stepsRemaining' =
      validateState state { stepsRemaining = stepsRemaining' }
    validateStep state (WriteFile file _) stepsRemaining' = do
      validateWriteFile state (T.unpack file) stepsRemaining'


validateFile :: State -> FilePath -> IO ()
validateFile state file = do
  case chrootDirectory state of
    Just _ -> pure ()
    Nothing ->
      case file of
        '/':_ -> fail $ "Absolute path " ++ file ++ " outside of chroot"
        _ -> pure ()


validateUrl :: String -> IO ()
validateUrl url = void $ parseRequest url
