module Lib.State
  ( State(..)
  , pushContext
  , pushDirectory
  , removeContext
  , removeDirectory
  ) where

import Lib.Context


data State = State
  { contextStack :: [Context]
  , directoryStack :: [FilePath]
  , startingDirectory :: FilePath
  , chrootDirectory :: Maybe FilePath
  , failed :: Bool
  }


pushContext :: State -> Context -> State
pushContext state context = state
  { contextStack = context : contextStack state
  }


removeContext :: State -> Context -> State
removeContext state context = state
  { contextStack = Prelude.filter (/= context) $ contextStack state
  }


pushDirectory :: State -> FilePath -> State
pushDirectory state dir = state
  { directoryStack = dir : directoryStack state
  }


removeDirectory :: State -> FilePath -> State
removeDirectory state dir =
  case directoryStack state of
    [] -> state
    (dir':dirs) ->
      if dir == dir'
        then state { directoryStack = dirs }
        else state { directoryStack = removeDir dirs }
  where
    removeDir :: [FilePath] -> [FilePath]
    removeDir [] = []
    removeDir (dir':dirs) =
      if dir' == dir then dirs else dir' : removeDir dirs
