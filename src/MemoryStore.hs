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

sources :: SourceStore a => a -> [FilePath] -> IO (Either Error [ByteString])
sources connection paths = do
  src <- sources' connection paths
  return $ sequence src
  where
    sources' _ [] = return []
    sources' conn (x:xs) = do
      src <- getSource conn x
      rest <- sources' conn xs
      return $ src:rest
