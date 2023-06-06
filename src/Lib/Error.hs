{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Error
  ( BuildException(..)
  , catchFail
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


catchFail :: State -> IO State -> IO State
catchFail state action = do
  catch action $ \(e :: BuildException) -> do
    putStrLn $ "  Caught exception: " ++ show e
    pure $ state { failed = True }
