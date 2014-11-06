{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON.Basic.Response
( Meta(..)
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
  } deriving (Show, Generic)

data Message = Message
  { h_meta :: Meta
  } deriving (Show, Generic)

instance FromJSON Meta
instance FromJSON Message

instance ToJSON Meta
instance ToJSON Message
