{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import EsbClient

-- JSON Modules
import qualified JSON.Login.Request as Login.Request
import qualified JSON.Basic.Response as Basic.Response

-- ESB Environment
host = "127.0.0.1"
port = 8900

-- Listening Recursion
listen :: Socket -> IO ()
listen sock = do
  -- In order to properly grab messages, there must be a
  -- corresponding JSON Module for the message (Response
  -- and/or Request). Haskell really enforces that we
  -- stick to a set data structure for each module.

  -- Grab just the meta portion so we can decide what
  -- kind of Data Structure to use.
  (meta, raw) <- esbGetMeta sock
  case Basic.Response.h_type meta of
    "sendMessage" -> do
      -- Do something with the data. This most likley
      -- means drilling down and further identifying the
      -- kind specific data structure.
      logger ("It's a message!")
    "heartbeat" -> do
      -- Do something with the heartbeat.
      logger ("It's a hearbeat!")
  
  -- Recurse
  listen sock

-- Initialization
main :: IO ()
main = do
  -- Get socket
  sock <- getSocket host port

  -- Login Request
  let loginData = Login.Request.Data { Login.Request.h_name = "haskell-test", Login.Request.h_subscriptions = [ "jkl" ] }
  esbLoginRequest sock loginData

  -- Login Response
  loginResponse <- esbLoginResponse sock
  case loginResponse of
    Left error -> logger ("! Login Error: " ++ error)
    Right response -> do
      logger ("Response: " ++ show response)

      -- Listen on socket
      listen sock
