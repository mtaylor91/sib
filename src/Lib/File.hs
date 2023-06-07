{-# LANGUAGE OverloadedStrings #-}
module Lib.File
  ( Lib.File.writeFile
  , resolveFile
  ) where


import Control.Exception (catch, throw)
import qualified Data.Text as T

import Lib.Context (Stack(..), chroot, directory, indent)
import Lib.Error (BuildException(..))


resolveFile :: Stack -> String -> FilePath
resolveFile stack path =
  case path of
    '/':rootRelative -> resolveRoot stack rootRelative
    relativePath -> resolveRelative stack relativePath


resolveRoot :: Stack -> FilePath -> FilePath
resolveRoot stack path =
  case chroot stack of
    Just dir -> T.unpack dir ++ "/" ++ path
    Nothing -> error "Unexpected root-relative path"


resolveRelative :: Stack -> FilePath -> FilePath
resolveRelative stack path =
  T.unpack (directory stack) ++ "/" ++ path


writeFile :: Stack -> T.Text -> T.Text -> IO ()
writeFile stack file content = do
  let realFile = resolveFile stack $ T.unpack file
  putStrLn $ indent stack ++ "Writing file: " ++ realFile
  putStrLn $ indent stack ++ "  " ++ T.unpack indentedContent
  catch (doWriteFile realFile) $ \e -> do
    let err = show (e :: IOError)
    putStrLn $ indent stack ++ "Failed to write file: " ++ err
    throw $ BuildIOException e
  where
    doWriteFile realFile = Prelude.writeFile realFile $ T.unpack content
    indentSpaces = "  " <> T.pack (indent stack)
    indentPrefix = "\n" <> indentSpaces
    indentedContent = T.intercalate indentPrefix $ T.lines content
