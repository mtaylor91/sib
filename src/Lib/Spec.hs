{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Lib.Spec
  ( loadSpec
  , SpecStep(..)
  , Spec(..)
  ) where

import Data.Text (Text)
import Data.Aeson.TH as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics

import Lib.Context


newtype InvalidSpec = DuplicateContext Text
  deriving (Show)


data SpecStep
  = RunCommand { command :: [Text] }
  | RunCommands { commands :: [[Text]] }
  | WriteFile { file :: Text, content :: Text }
  | DownloadFile { file :: Text, sha512 :: Text, url :: Text }
  | EnterContext { enter :: Text }
  | LeaveContext { leave :: Text }
  deriving (Show, Generic)


$(A.deriveJSON A.defaultOptions
  { sumEncoding = A.UntaggedValue
  } ''SpecStep)


data Spec = Spec
  { name :: Text
  , steps :: [SpecStep]
  , contexts :: [Context]
  } deriving (Show, Generic)


instance Y.FromJSON Spec


loadSpec :: FilePath -> IO Spec
loadSpec path = do
  specYaml <- BS.readFile path
  spec <- Y.decodeThrow specYaml
  let contextNames = map contextName $ contexts spec
  case findDuplicates [] contextNames contextNames of
    [] -> pure spec
    duplicate:_ -> fail $ "Duplicate context(s): " ++ show duplicate
  where
    findDuplicates :: Eq a => [a] -> [a] -> [a] -> [a]
    findDuplicates duplicates [] _ = duplicates
    findDuplicates duplicates (x:xs) ys =
      case Prelude.filter (== x) ys of
        [] -> findDuplicates duplicates xs ys
        [_] -> findDuplicates duplicates xs ys
        _ -> findDuplicates (x:duplicates) xs ys
