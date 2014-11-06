{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Haskell version of the Help.com ESB Client
-- v0.0.1

module EsbClient
( getSocket
, sendSocketDataRaw
, sendSocketData
, readSocketDataRaw
, readSocketData
, esbGetMeta
, esbLoginRequest
, esbLoginResponse
, logger
, Socket
) where

-- Base Modules
import System.IO
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

sendSocketDataRaw :: Socket -> [Char] -> IO ()
sendSocketDataRaw sock json = do
  send sock (json ++ "\n")
  logger ("+ Raw Send: " ++ json)

sendSocketData :: Socket -> Login.Request.Meta -> Login.Request.Data -> IO ()
sendSocketData sock meta payload = do
  let message = Login.Request.Message { Login.Request.h_meta = meta, Login.Request.h_data = payload }
  let stringified = replace "h_" "" (C.unpack (encode (message)))
  sendSocketDataRaw sock stringified

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

-- ESB Specific Functions
esbGetMeta :: Socket -> IO (Basic.Response.Meta, C.ByteString)
esbGetMeta sock = do
  bytes <- readSocketData sock
  let response = eitherDecode bytes
  case response of
    Left error -> do
      logger ("! Error: " ++ error)
      let meta = Basic.Response.Meta { Basic.Response.h_type = "error", Basic.Response.h_id = "000" }
      return (meta, bytes)
    Right message -> do
      let meta = Basic.Response.h_meta message
      logger ("Message: " ++ show meta)
      return (meta, bytes)

esbLoginRequest :: Socket -> Login.Request.Data -> IO ()
esbLoginRequest sock payload = do
  uuid <- nextRandom
  let meta = Login.Request.Meta { Login.Request.h_type = "login", Login.Request.h_id = toString uuid }
  sendSocketData sock meta payload

esbLoginResponse :: Socket -> IO (Either String Login.Response.Message)
esbLoginResponse sock = do
  bytes <- readSocketData sock
  return $ eitherDecode $ bytes
