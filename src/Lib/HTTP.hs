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
import System.Directory (doesFileExist)

import Lib.Error
import Lib.State


-- Download a file from a URL.
downloadFile :: State -> FilePath -> String -> String -> IO State
downloadFile state destination sha512sum url = do
  exists <- doesFileExist destination
  if exists
    then doCheckFile
    else doDownloadFile
  where
    doCheckFile :: IO State
    doCheckFile = do
      sha512sum' <- sha512File destination
      if sha512sum == sha512sum'
        then do
          putStrLn $
            "File " ++ destination ++ " already exists and has correct SHA512 checksum"
          pure state
        else do
          putStrLn $
            "File " ++ destination ++ " already exists but has incorrect SHA512 checksum"
          doDownloadFile
    doDownloadFile :: IO State
    doDownloadFile = do
      putStrLn $ "Downloading file " ++ destination
      putStrLn $ "  " ++ url
      manager <- newManager tlsManagerSettings
      request <- parseRequest url
      withResponse request manager handleResponse
    handleResponse :: Response BodyReader -> IO State
    handleResponse response = do
      let headers = responseHeaders response
          body = responseBody response
          status = responseStatus response
          maybeContentLengthHeader = lookup hContentLength headers
          maybeContentLength = maybeContentLengthHeader >>= BS8.readInt
      case (statusCode status, maybeContentLength) of
        (codeOK, Just (contentLength, _)) | codeOK >= 200 && codeOK < 300 -> do
          writeToFile state destination sha512sum 0 contentLength body
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
writeToFile state destination sha512sum downloaded total body = do
  bs <- brRead body
  case (BS.null bs, downloaded) of
    (False, _) -> do
      BS.appendFile destination bs
      let downloaded' = downloaded + BS.length bs
      writeToFile state destination sha512sum downloaded' total body
    (True, dl) | dl == total -> do
      sha512sum' <- sha512File destination
      if sha512sum == sha512sum'
        then pure state
        else throw $ BuildDownloadError "Incorrect SHA512 checksum"
    (True, _) ->
      throw $ BuildDownloadError "Incomplete response"
