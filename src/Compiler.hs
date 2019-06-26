{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import MemoryStore
import Data.ByteString

class Compiler a where
  compile :: SourceStore s => a -> s -> FilePath -> IO ByteString
