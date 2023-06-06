{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.HTTP
  ( downloadFile
  ) where


import Control.Exception (throw)
import Crypto.Hash
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist, removeFile)

import Lib.Error
import Lib.File (resolveFile)
import Lib.State


-- Download a file from a URL.
downloadFile :: State -> FilePath -> String -> String -> IO State
downloadFile state destination sha512sum url = do
  let filename = resolveFile state destination
  exists <- doesFileExist filename
  if exists
    then doCheckFile filename
    else doDownloadFile filename
  where
    doCheckFile :: FilePath -> IO State
    doCheckFile filename = do
      sha512sum' <- sha512File filename
      if sha512sum == sha512sum'
        then do
          putStrLn $
            "File " ++ filename ++ " already exists and has correct SHA512 checksum"
          pure state
        else do
          putStrLn $
            "File " ++ filename ++ " already exists but has incorrect SHA512 checksum"
          removeFile filename
          doDownloadFile filename
    doDownloadFile :: FilePath -> IO State
    doDownloadFile filename = do
      putStrLn $ "Downloading file " ++ filename
      putStrLn $ "  " ++ url
      manager <- newManager tlsManagerSettings
      request <- parseRequest url
      withResponse request manager $ handleResponse filename
    handleResponse :: FilePath -> Response BodyReader -> IO State
    handleResponse filename response = do
      let headers = responseHeaders response
          body = responseBody response
          status = responseStatus response
          maybeContentLengthHeader = lookup hContentLength headers
          maybeContentLength = maybeContentLengthHeader >>= BS8.readInt
      case (statusCode status, maybeContentLength) of
        (codeOK, Just (contentLength, _)) | codeOK >= 200 && codeOK < 300 -> do
          writeToFile state filename sha512sum 0 contentLength body
        (_, Nothing) -> do
          throw $ BuildDownloadError "No Content-Length header"
        (codeErr, _) -> do
          throw $ BuildDownloadError $ "HTTP error " ++ show codeErr


sha512File :: FilePath -> IO FilePath
sha512File filename = do
  lbs <- LBS.readFile filename
  let digest = hashlazy lbs :: Digest SHA512
  pure $ show digest


-- Write an HTTP response to disk
writeToFile :: State -> FilePath -> String -> Int -> Int -> BodyReader -> IO State
writeToFile state filename sha512sum downloaded total body = do
  bs <- brRead body
  case (BS.null bs, downloaded) of
    (False, _) -> do
      BS.appendFile filename bs
      let downloaded' = downloaded + BS.length bs
      writeToFile state filename sha512sum downloaded' total body
    (True, dl) | dl == total -> do
      sha512sum' <- sha512File filename
      if sha512sum == sha512sum'
        then pure state
        else throw $ BuildDownloadError "Incorrect SHA512 checksum"
    (True, _) ->
      throw $ BuildDownloadError "Incomplete response"
