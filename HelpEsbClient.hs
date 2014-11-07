{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Haskell version of the Help.com ESB Client
-- v0.1.0

module HelpEsbClient
-- Classes
( EsbSend
, EsbRecieve
-- Raw Exports
, getSocket
, sendSocketData
, readSocketDataRaw
, readSocketData
-- ESB Functions
, esbSend
, esbRecieve
, esbInit
, esbListen
-- Utility
, encode
, decode
, eitherDecode
, logger
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

-- Classes
class EsbSend a where
  esbSend :: Socket -> a -> IO ()

class EsbRecieve a where
  esbRecieve :: Socket -> a -> IO ()

-- Logging
logger :: String -> IO ()
logger out = do
  let logPrefix = "# "
  putStrLn $ logPrefix ++ out

-- Socket Specific Functions
getSocket :: String -> Int -> IO Socket
getSocket host port = withSocketsDo $ do
  (serveraddr:_) <- getAddrInfo Nothing (Just host) (Just (show port))
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr) >> return sock

sendSocketData :: Socket -> C.ByteString -> IO ()
sendSocketData sock bytes = do
  let json = replace "h_" "" (C.unpack bytes)
  send sock (json ++ "\n")
  logger ("+ Raw Send: " ++ json)

readSocketDataRaw :: Socket -> IO [Char]
readSocketDataRaw sock = do
  message <- recv sock 1024
  logger ("+ Raw Read: " ++ message)
  return message

readSocketData :: Socket -> IO (C.ByteString)
readSocketData sock = do
  raw <- readSocketDataRaw sock
  let fixed = replace ",\"" ",\"h_" (replace "{\"" "{\"h_" raw)
  return $ C.pack fixed

-- Send Socket Instances
instance EsbSend Login.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = Login.Request.Meta { Login.Request.h_type = "login", Login.Request.h_id = toString uuid }
    let message = Login.Request.Message { Login.Request.h_meta = meta, Login.Request.h_data = payload }
    sendSocketData sock (encode message)

-- Recieve Socket Instances
instance EsbRecieve Login.Response.Message where
  esbRecieve sock message = do
    let payload = Login.Response.h_meta message
    case Login.Response.h_result payload of
      "SUCCESS" -> logger ("Successfully logged in.")
      _ -> error "! Failed to login!"

-- Initialization Instances
esbInit :: Text -> [Text] -> String -> Int -> IO Socket
esbInit name subscriptions host port = do
  sock <- getSocket host port
  let loginData = Login.Request.Data { Login.Request.h_name = name, Login.Request.h_subscriptions = subscriptions }
  esbSend sock loginData
  return sock

-- Essential Listening Logic
esbListen :: Socket -> IO (C.ByteString)
esbListen sock = do
  bytes <- readSocketData sock

  case eitherDecode bytes :: (Either String Login.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieve sock response

  return bytes
