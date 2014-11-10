{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON.API.EventGroup.Post.Request
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
  { h_eventGroupType :: [Char]
  , h_eventGroupId :: Text
  , h_ownerType :: Text
  , h_ownerId :: [Char]
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
  toJSON (Data h_eventGroupType h_eventGroupId h_ownerType h_ownerId) =
    object [ "h_type" .= h_eventGroupType
           , "h_id" .= h_eventGroupId
           , "h_ownerType" .= h_ownerType
           , "h_ownerId" .= h_ownerId
           ]
instance ToJSON Message
