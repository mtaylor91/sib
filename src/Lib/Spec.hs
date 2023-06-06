{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Spec
  ( Context(..)
  , CommandOrCommands(..)
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
  | RunCommands { commands :: CommandOrCommands }
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
  spec <- Y.decodeThrow specYaml
  let contextNames = getContextNames [] $ steps spec
  case findDuplicates [] contextNames contextNames of
    [] -> verifyLeaves contextNames spec
    duplicate:_ -> fail $ "Duplicate context(s): " ++ show duplicate
  where
    findDuplicates :: Eq a => [a] -> [a] -> [a] -> [a]
    findDuplicates duplicates [] _ = duplicates
    findDuplicates duplicates (x:xs) ys =
      case Prelude.filter (== x) ys of
        [] -> findDuplicates duplicates xs ys
        [_] -> findDuplicates duplicates xs ys
        _ -> findDuplicates (x:duplicates) xs ys
    getContextNames :: [Text] -> [Step] -> [Text]
    getContextNames names [] = names
    getContextNames names (step:steps) =
      case step of
        EnterContext name _ -> getContextNames (name:names) steps
        _ -> getContextNames names steps
    verifyLeaves :: [Text] -> Spec -> IO Spec
    verifyLeaves contextNames spec =
      case verifyLeaves' contextNames $ steps spec of
        [] -> return spec
        leaves -> fail $ "Leaves without matching enter: " ++ show leaves
    verifyLeaves' :: [Text] -> [Step] -> [Text]
    verifyLeaves' _ [] = []
    verifyLeaves' contextNames (step:steps) =
      case step of
        LeaveContext name ->
          case contextNames of
            contextName:contextNames' ->
              if name == contextName
                then verifyLeaves' contextNames' steps
                else fail $
                  "Cannot leave context " ++ show name ++
                  " from context " ++ show contextName
            [] -> fail $ "Leave without matching enter: " ++ show name
        _ -> verifyLeaves' contextNames steps
