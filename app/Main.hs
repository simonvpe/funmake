{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified ElmArchitecture
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init)
import qualified Database.Redis
import qualified RedisStore
import qualified MemoryStore
import Data.ByteString hiding (init, getLine, putStrLn)
import Data.ByteString.UTF8 (toString)

main :: IO ()
main = ElmArchitecture.run Config
  { _init = init
  , _update = update
  }

-- STATE

data Model = Model
  { counter :: Int
  , content :: String
  , store :: Maybe Database.Redis.Connection
  }

data Msg
  = DoNothing
  | SetContent String
  | SetStore Database.Redis.Connection
  | DecreaseCounter

-- INIT

init :: (Model, Cmd Msg)
init =
  (Model { counter = 10
         , content = "No content"
         , store   = Nothing
         }
  , [ -- Immidiate IO action
      SetStore <$> redisConnect
      -- Async tasks
    , SetContent <$> waitFor 2 "Two"
    , SetContent <$> httpGet "https://hackage.haskell.org"
    , SetContent <$> httpGet "https://elm.lang.org"
    , return DecreaseCounter
    ]
  )

-- UPDATE

update :: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing ->
      (model
      , []
      )

    SetStore store ->
      (model { store = Just store }
      , [ SetContent . unwrap <$> MemoryStore.getSource store ("cpp/a.cpp" :: FilePath) ]
      )

    DecreaseCounter ->
      if counter model == 0 then
        (model
        , []
        )
      else
        (model {counter = counter model - 1}
        , [ waitFor 1 DecreaseCounter
          , ignoreResult $ putStrLn $ show $ content model
          ]
        )

    SetContent content ->
      ( model { content = content }
      , []
      )

ignoreResult :: IO a -> IO Msg
ignoreResult io = io >> return DoNothing

-- Simulate http

httpGet :: String -> IO String
httpGet url =
  case url of
    "https://hackage.haskell.org" -> waitFor 3 "Hackage website content"
    "https://elm.lang.org" -> waitFor 4 "Elm website content"
    _ -> waitFor 4 "404"

-- Return a value after a few seconds
waitFor :: Int -> a -> IO a
waitFor seconds val = do
  threadDelay $ seconds * 1000000
  return val

redisConnect :: IO(Database.Redis.Connection)
redisConnect = Database.Redis.checkedConnect Database.Redis.defaultConnectInfo

unwrap :: Either a ByteString -> String
unwrap thing = case thing of
  Left _ -> "ERROR"
  Right s -> toString s
