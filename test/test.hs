{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import EsbClient

-- JSON Modules
import qualified JSON.Login.Request as Login.Request

-- ESB Environment
host = "127.0.0.1"
port = 8900

-- Listening Recursion
listen :: Socket -> IO ()
listen sock = do
  -- Grab the raw message
  -- In order to properly grab messages, there must be a
  -- corresponding JSON Module for the message (Response
  -- and/or Request). Haskell really enforces that we
  -- stick to a set data structure for each module.
  readSocketDataRaw sock

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
