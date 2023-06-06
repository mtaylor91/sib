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
  validateSpec spec


validateSpec :: Spec -> IO Spec
validateSpec spec = do
  let enterContextNames = collectEnterContextNames [] $ steps spec
  case findConflictingContexts [] enterContextNames enterContextNames of
    duplicate:_ -> fail $ "Duplicate context(s): " ++ show duplicate
    [] -> do
      verifyLeaveContextSteps enterContextNames $ steps spec
      pure spec
  where
    collectEnterContextNames :: [Text] -> [Step] -> [Text]
    collectEnterContextNames names [] = names
    collectEnterContextNames names (step:steps) =
      case step of
        EnterContext name _ -> collectEnterContextNames (name:names) steps
        _ -> collectEnterContextNames names steps
    findConflictingContexts :: Eq a => [a] -> [a] -> [a] -> [a]
    findConflictingContexts duplicates [] _ = duplicates
    findConflictingContexts duplicates (x:xs) ys =
      case Prelude.filter (== x) ys of
        [] -> findConflictingContexts duplicates xs ys
        [_] -> findConflictingContexts duplicates xs ys
        _ -> findConflictingContexts (x:duplicates) xs ys
    verifyLeaveContextSteps :: [Text] -> [Step] -> IO ()
    verifyLeaveContextSteps [] [] = return ()
    verifyLeaveContextSteps (name:_) [] =
      fail $ "Context " ++ show name ++ " was never left"
    verifyLeaveContextSteps [] (step:steps) =
      case step of
        LeaveContext name ->
          fail $ "Leave without matching enter: " ++ show name
        _ -> verifyLeaveContextSteps [] steps
    verifyLeaveContextSteps (name:names) (step:steps) =
      case step of
        LeaveContext name' ->
          if name == name'
            then verifyLeaveContextSteps names steps
            else fail $
              "Cannot leave context " ++ show name' ++
              " from context " ++ show name
        _ -> verifyLeaveContextSteps (name:names) steps
