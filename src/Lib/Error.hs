{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Error
  ( BuildException(..)
  , buildIOError
  , catchFail
  , catchFailIO
  ) where

import Control.Exception

import Lib.State


data BuildException
  = BuildFailed
  | BuildCommandFailed String
  | BuildDownloadError String
  | BuildInterrupted
  | BuildIOException IOException
  deriving (Show)


instance Exception BuildException


buildIOError :: IOException -> BuildException
buildIOError = BuildIOException


catchFail :: State -> IO State -> IO State
catchFail state action = do
  catch action $ \(e :: BuildException) -> do
    putStrLn $ "  Caught exception: " ++ show e
    pure $ state { failed = True }


catchFailIO :: State -> IO State -> IO State
catchFailIO state action = do
  catch action $ \(e :: IOException) -> do
    putStrLn $ "  Caught exception: " ++ show e
    pure $ state { failed = True }
