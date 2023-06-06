{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.State
  ( State(..)
  , chrootDirectory
  , contextDirectory
  , popContext
  , pushContext
  ) where

import qualified Data.Text as T

import Lib.Spec


data State = State
  { spec :: Spec
  , contextStack :: [(T.Text, Context)]
  , startingDirectory :: FilePath
  , stepsRemaining :: [Step]
  , failed :: Bool
  } deriving (Show)


chrootDirectory :: State -> Maybe FilePath
chrootDirectory state = T.unpack <$> findCtxChroot (contextStack state) where
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


pushContext :: State -> T.Text -> Context -> State
pushContext state name context = state
  { contextStack = (name, context) : contextStack state
  }
