{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Lib.Spec
  ( Context(..)
  , CommandOrCommands(..)
  , CommandOrCommandsStep(..)
  , Step(..)
  , Spec(..)
  , loadSpec
  ) where

import Data.Text (Text)
import Data.Aeson.TH as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics


newtype InvalidSpec = DuplicateContext Text
  deriving (Show)


data CommandOrCommands
  = Command [Text]
  | Commands [[Text]]
  deriving (Eq, Show, Generic)


$(A.deriveJSON A.defaultOptions { sumEncoding = A.UntaggedValue } ''CommandOrCommands)


data CommandOrCommandsStep
  = CommandStep { command :: CommandOrCommands }
  | CommandsStep { commands :: CommandOrCommands }
  deriving (Eq, Show, Generic)


$(A.deriveJSON A.defaultOptions
  { sumEncoding = A.UntaggedValue } ''CommandOrCommandsStep)


data Context
  = Chroot { chroot :: Text }
  | Directory { directory :: Text }
  | BracketCommands { onEnter :: CommandOrCommands, onLeave :: CommandOrCommands }
  deriving (Eq, Show, Generic)


$(A.deriveJSON A.defaultOptions { sumEncoding = A.UntaggedValue } ''Context)


data Step
  = DownloadFile { file :: Text, sha512 :: Text, url :: Text }
  | EnterContext { name :: Text, enter :: Context }
  | LeaveContext { leave :: Text }
  | RunCommands CommandOrCommandsStep
  | WriteFile { file :: Text, content :: Text }
  deriving (Show, Generic)


$(A.deriveJSON A.defaultOptions { sumEncoding = A.UntaggedValue } ''Step)


data Spec = Spec
  { name :: Text
  , steps :: [Step]
  } deriving (Show, Generic)


instance Y.FromJSON Spec


loadSpec :: FilePath -> IO Spec
loadSpec path = do
  specYaml <- BS.readFile path
  Y.decodeThrow specYaml
