module Lib.HTTP
  ( downloadFile
  ) where


import Data.ByteString as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Lib.State


-- Download a file from a URL.
downloadFile :: State -> String -> String -> IO State
downloadFile state destination url = do
  putStrLn $ "Downloading file " ++ destination
  putStrLn $ "  " ++ url
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  withResponse request manager handleResponse
  where
    handleResponse :: Response BodyReader -> IO State
    handleResponse response =
      let body = responseBody response
       in writeToFile state destination 0 body


-- Write an HTTP response to disk
writeToFile :: State -> String -> Int -> BodyReader -> IO State
writeToFile state destination downloaded body = do
  bs <- brRead body
  if BS.null bs
    then return state
    else do
      BS.appendFile destination bs
      let downloaded' = downloaded + BS.length bs
      putStrLn $ "  " ++ show downloaded' ++ " bytes"
      writeToFile state destination downloaded' body
