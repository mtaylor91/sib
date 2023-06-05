module Lib
    ( buildImageFromDefaultSpec
    ) where

import Lib.Build
import Lib.Spec


defaultSpecPath :: IO FilePath
defaultSpecPath = pure "spec.yml"


buildImageFromDefaultSpec :: IO ()
buildImageFromDefaultSpec =
  defaultSpecPath >>= loadSpec >>= buildImage
