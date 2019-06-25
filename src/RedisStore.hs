{-# LANGUAGE OverloadedStrings #-}

module RedisStore where

import MemoryStore
import Database.Redis
import Control.Monad.IO.Class
import Data.ByteString

instance SourceStore Connection where
  getSource conn key = runRedis conn $ do
    value <- get key
    return $ case value of
      Left _ -> Left (NetworkError "HELLO")
      Right ok -> case ok of
        Nothing -> Left (NotFoundError key)
        Just y -> Right y
  
