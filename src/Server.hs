{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
-- import           Servant.Types.SourceT
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8


type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position -- e.g. /position/1/2
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage          -- e.g. /hello?name=albert
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email          -- e.g. /marketing (POST)


data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position


newtype HelloMessage = HelloMessage { msg :: String } deriving Generic

instance ToJSON HelloMessage


data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo


data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving Generic

instance ToJSON Email


emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from'    = "great@company.com"
    to'      = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body'    = "Hi " ++ clientName c ++ ",\n\n"
            ++ "Since you've recently turned " ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"


server :: Server API
server = position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = pure (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello mname = pure . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n  -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = pure (emailForClient clientinfo)


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


main :: IO ()
main = run 8081 app
