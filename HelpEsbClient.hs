{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{-|
Module : HelpEsbClient
Description: Haskell version of the Help.com ESB Client
Copyright: (c) Help.com, LLC, 2014
License: MIT
Maintainer: alex.martin@help.com
Stability: Stable
Portability: UNIX
-}
module HelpEsbClient (
-- * Classes
  EsbSend
, EsbRecieve
-- * Raw Exported Functions
, getSocket
, sendSocketData
, readSocketDataRaw
, readSocketData
-- * ESB Functions
, esbSend
, esbRecieve
, esbInit
, esbListen
-- * Utility Functions
, encode
, decode
, eitherDecode
, logger
-- * Utility Types
, Socket
, module Data.UUID
, module Data.UUID.V4
, module System.Environment
) where

-- Base Modules
import System.IO
import System.Environment
import Network.Socket
import Control.Exception
import GHC.Generics
import Data.Text hiding (replace)
import Data.List.Utils
import Data.UUID
import Data.UUID.V4
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C

-- JSON Modules
import qualified JSON.Basic.Response as Basic.Response
import qualified JSON.Login.Request as Login.Request
import qualified JSON.Login.Response as Login.Response
import qualified JSON.API.EventGroup.Post.Request as EventGroup.Post.Request
import qualified JSON.API.Event.Post.Request as Event.Post.Request

-- Classes
-- | The 'EsbSend' class determines how a message should be sent to the ESB.
class EsbSend a where
  -- | The 'esbSend' method takes a socket and writes somekind of payload.
  esbSend :: Socket -- ^ The socket connection.
    -> a -- ^ The payload.
    -> IO () -- ^ Any IO output.

-- | The 'EsbRecieve' class determines how a message from the ESB should be
-- recieved.
class EsbRecieve a where
  -- | The 'esbRecieve' method takes a socket and reads somekind of payload.
  esbRecieve :: Socket -- ^ The socket connection.
    -> a -- ^ The payload.
    -> IO () -- ^ Any IO output.

-- | The 'logger' function simply logs out in a consistent way. Will be
-- updated to be more robust.
logger :: String -- ^ Messaged to be logged.
  -> IO () -- ^ IO output.
logger out = do
  let logPrefix = "# "
  putStrLn $ logPrefix ++ out

-- Socket Specific Functions
-- | The 'getSocket' function takes a host and port and connects to and
-- returns the socket.
getSocket :: String -- ^ Host address.
  -> Int -- ^ Host port.
  -> IO Socket -- ^ The socket connection.
getSocket host port = withSocketsDo $ do
  (serveraddr:_) <- getAddrInfo Nothing (Just host) (Just (show port))
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr) >> return sock

-- | The 'sendSocketData' function accepts a socket and bytes, converts the
-- bytes to cleaned up JSON, and writes the JSON to the socket.
sendSocketData :: Socket -- ^ The socket connection.
  -> C.ByteString -- ^ The JSON bytestring payload.
  -> IO () -- ^ Any IO output.
sendSocketData sock bytes = do
  let json = replace "h_" "" (C.unpack bytes)
  send sock (json ++ "\n")
  logger ("+ Raw Send: " ++ json)

-- | The 'readSocketDataRaw' function accepts a socket and grabs whatever
-- data might be in the latest message.
readSocketDataRaw :: Socket -- ^ The socket connection.
  -> IO [Char] -- ^ Any IO output.
readSocketDataRaw sock = do
  message <- recv sock 1024
  logger ("+ Raw Read: " ++ message)
  return message

-- | The 'readSocketData' function accepts a socket, reads and cleans up
-- any JSON for parsing, and returns the bytes of JSON.
readSocketData :: Socket -- ^ The socket connection.
  -> IO (C.ByteString) -- ^ The JSON bytestring payload.
readSocketData sock = do
  raw <- readSocketDataRaw sock
  let fixed = replace ",\"" ",\"h_" (replace "{\"" "{\"h_" raw)
  return $ C.pack fixed

-- Send Socket Instances
-- | The 'EsbSend' instance for a login request.
instance EsbSend Login.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = Login.Request.Meta {
        Login.Request.h_type = "login"
      , Login.Request.h_id = toString uuid
      }
    let message = Login.Request.Message {
        Login.Request.h_meta = meta
      , Login.Request.h_data = payload
      }
    sendSocketData sock (encode message)
    logger ("Login Request: " ++ show message)

-- | The 'EsbSend' instance for an Event Group post request.
instance EsbSend EventGroup.Post.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = EventGroup.Post.Request.Meta {
        EventGroup.Post.Request.h_type = "sendMessage"
      , EventGroup.Post.Request.h_id = toString uuid
      , EventGroup.Post.Request.h_group = "api-messages"
      }
    let message = EventGroup.Post.Request.Message {
        EventGroup.Post.Request.h_meta = meta
      , EventGroup.Post.Request.h_data = payload
      }
    sendSocketData sock (encode message)
    logger ("EventGroup API Request: " ++ show message)

-- | The 'EsbSend' instance for an Event post request.
instance EsbSend Event.Post.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = Event.Post.Request.Meta {
        Event.Post.Request.h_type = "sendMessage"
      , Event.Post.Request.h_id = toString uuid
      , Event.Post.Request.h_group = "api-messages"
      }
    let message = Event.Post.Request.Message {
        Event.Post.Request.h_meta = meta
      , Event.Post.Request.h_data = payload
      }
    sendSocketData sock (encode message)
    logger ("Event API Request: " ++ show message)

-- Recieve Socket Instances
-- | The 'EsbRecieve' instance for a Login response.
instance EsbRecieve Login.Response.Message where
  esbRecieve sock message = do
    let payload = Login.Response.h_meta message
    case Login.Response.h_result payload of
      "SUCCESS" -> logger ("Successfully logged in.")
      _ -> error "! Failed to login!"

-- Initialization Instances
-- | The 'esbInit' function initializes the socket connection and logs
-- into the ESB.
esbInit :: Text -- ^ Group name.
  -> [Text] -- ^ Subscriptions.
  -> String -- ^ Host address.
  -> Int -- ^ Host port.
  -> IO Socket -- ^ The socket connection.
esbInit name subscriptions host port = do
  sock <- getSocket host port
  let loginData = Login.Request.Data { Login.Request.h_name = name, Login.Request.h_subscriptions = subscriptions }
  esbSend sock loginData
  return sock

-- Essential Listening Logic
-- | The 'esbListen' function performs all essential listening logic
-- for any ESB client.
esbListen :: Socket -- ^ The socket connection.
  -> IO (C.ByteString) -- ^ The JSON bytestring payload.
esbListen sock = do
  bytes <- readSocketData sock

  case eitherDecode bytes :: (Either String Login.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieve sock response

  return bytes
