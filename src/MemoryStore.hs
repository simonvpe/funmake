module MemoryStore where

import Data.ByteString

data Error
  = NetworkError ByteString
  | NotFoundError ByteString
  deriving (Show)
  

class SourceStore a where
  getSource :: a -> ByteString -> IO(Either Error ByteString)
