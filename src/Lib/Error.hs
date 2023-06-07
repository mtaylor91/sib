{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Error
  ( BuildException(..)
  ) where

import Control.Exception


data BuildException
  = BuildFailed
  | BuildCommandFailed String
  | BuildDownloadError String
  | BuildInterrupted
  | BuildIOException IOException
  deriving (Show)


instance Exception BuildException
