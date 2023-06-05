{-# LANGUAGE OverloadedStrings #-}
module Lib.File
  ( Lib.File.writeFile
  ) where


import Control.Exception (catch)
import qualified Data.Text as T

import Lib.State


resolveFile :: State -> T.Text -> FilePath
resolveFile state path =
  case T.unpack path of
    '/':rootRelative -> resolveRoot state rootRelative
    relativePath -> resolveRelative state relativePath


resolveRoot :: State -> FilePath -> FilePath
resolveRoot state path =
  case chrootDirectory state of
    Just chroot -> chroot ++ "/" ++ path
    Nothing -> path


resolveRelative :: State -> FilePath -> FilePath
resolveRelative state path =
  case directoryStack state of
    [] -> path
    (dir:_) -> dir ++ "/" ++ path


writeFile :: State -> T.Text -> T.Text -> IO State
writeFile state file content = if failed state then pure state else do
  let realFile = resolveFile state file
  putStrLn ""
  putStrLn $ "Writing file: " ++ realFile
  putStrLn $ "  " ++ T.unpack indentedContent
  catch (doWriteFile realFile) $ \e -> do
    let err = show (e :: IOError)
    putStrLn $ "  Failed to write file: " ++ err
    pure $ state { failed = True }
  where
    doWriteFile realFile = do
      Prelude.writeFile realFile $ T.unpack content
      pure state
    indentedContent = T.intercalate "\n  " $ T.lines content
