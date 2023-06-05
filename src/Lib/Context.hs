{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Lib.Context
  ( Context(..)
  , contextName
  ) where

import Data.Aeson.TH as A
import Data.Text
import GHC.Generics


data Context
  = Chroot { chroot :: Text }
  | Directory { directory :: Text }
  | BracketCommands { name :: Text, enter :: [[Text]], leave :: [[Text]] }
  deriving (Eq, Show, Generic)


$(A.deriveJSON A.defaultOptions
  { sumEncoding = A.UntaggedValue
  } ''Context)


contextName :: Context -> Text
contextName (Chroot _) = "chroot"
contextName (Directory d) = d
contextName (BracketCommands n _ _) = n
