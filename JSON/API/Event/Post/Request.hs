{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON.API.Event.Post.Request
( Meta(..)
, Data(..)
, Message(..)
, encode
, decode
, eitherDecode
) where

import GHC.Generics
import Data.Text
import Data.Aeson

data Meta = Meta
  { h_type :: Text
  , h_id :: [Char]
  , h_group :: Text
  } deriving (Show, Generic)

data Data = Data
  { h_createdAt :: Int
  , h_content :: Text
  , h_eventType :: Text
  , h_senderType :: Text
  , h_senderId :: [Char]
  , h_customerId :: [Char]
  } deriving (Show, Generic)

data Message = Message
  { h_meta :: Meta
  , h_data :: Data
  } deriving (Show, Generic)

instance FromJSON Meta
instance FromJSON Data
instance FromJSON Message

instance ToJSON Meta
instance ToJSON Data where
  toJSON (Data h_createdAt h_content h_eventType h_senderType h_senderId h_customerId) =
    object [ "h_createdAt" .= h_createdAt
           , "h_content" .= h_content
           , "h_type" .= h_eventType
           , "h_senderType" .= h_senderType
           , "h_senderId" .= h_senderId
           , "h_customerId" .= h_customerId
           ]
instance ToJSON Message
