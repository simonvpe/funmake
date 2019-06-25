{-# LANGUAGE OverloadedStrings #-}

module RedisStore where

import MemoryStore
import Data.ByteString
import Control.Monad.IO.Class
import Database.Redis
import Data.ByteString.Char8 as C8

instance SourceStore Connection where
  getSource conn filepath = runRedis conn $ do
    let filename = C8.pack filepath
    result <- hget "src" filename
    return $ case result of
      Left err -> Left $ BackendError $ C8.pack $ show err
      Right Nothing -> Left $ NotFoundError filename
      Right (Just x) -> Right x

  setSource conn filepath = runRedis conn $ do
    let filename = C8.pack filepath
    content <- liftIO $ Data.ByteString.readFile filepath
    liftIO $ Data.ByteString.putStrLn content
    result <- hset "src" filename content
    return $ case result of
      Left err -> Left(BackendError $ C8.pack $ show err)
      Right False -> Left $ BackendError "Insertion failed"
      Right True -> Right True
