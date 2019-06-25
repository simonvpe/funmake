{-# LANGUAGE OverloadedStrings #-}
module MemoryStore where

import Data.ByteString
import Data.ByteString.Char8 as C8
import Control.Monad.IO.Class

data Error
  = BackendError ByteString
  | NotFoundError ByteString
  deriving (Show)

class SourceStore a where
  getSource :: a -> FilePath -> IO(Either Error ByteString)
  setSource :: a -> FilePath -> IO(Either Error Bool)
