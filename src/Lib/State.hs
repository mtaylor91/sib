{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.State
  ( State(..)
  , contextChrootDirectory
  , contextDirectory
  , popContext
  , popDirectory
  , pushContext
  , pushDirectory
  ) where

import qualified Data.Text as T

import Lib.Spec


data State = State
  { contextStack :: [(T.Text, Context)]
  , directoryStack :: [FilePath]
  , startingDirectory :: FilePath
  , chrootDirectory :: Maybe FilePath
  , failed :: Bool
  } deriving (Show)


contextChrootDirectory :: State -> Maybe FilePath
contextChrootDirectory state = T.unpack <$> findCtxChroot (contextStack state) where
  findCtxChroot [] = Nothing
  findCtxChroot ((_, context):contexts) =
    case context of
      Chroot dir -> Just dir
      _ -> findCtxChroot contexts


contextDirectory :: State -> Maybe FilePath
contextDirectory state = T.unpack <$> findCtxDir (contextStack state) where
  findCtxDir [] = Nothing
  findCtxDir ((_, context):contexts) =
    case context of
      Directory dir -> Just dir
      _ -> findCtxDir contexts


popContext :: State -> (State, Maybe (T.Text, Context))
popContext state =
  case contextStack state of
    [] -> (state, Nothing)
    (context:contexts) -> (state { contextStack = contexts }, Just context)


popDirectory :: State -> (State, Maybe FilePath)
popDirectory state =
  case directoryStack state of
    [] -> (state, Nothing)
    (dir:dirs) -> (state { directoryStack = dirs }, Just dir)


pushContext :: State -> T.Text -> Context -> State
pushContext state name context = state
  { contextStack = (name, context) : contextStack state
  }


pushDirectory :: State -> FilePath -> State
pushDirectory state dir = state
  { directoryStack = dir : directoryStack state
  }
