{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import HelpEsbClient

-- ESB Environment
-- Will first try to use the ESB environment variable.
-- If that fails, it will default to 127.0.0.1:8900.
host = Nothing
port = Nothing

-- Listening Recursion
listen :: Socket -> IO ()
listen sock = do
  -- Perform essential listening logic
  bytes <- esbListen sock

  listen sock

-- Initialization
main :: IO ()
main = do
  -- Connect to socket and login
  sock <- esbInit "haskell-test" [ ] host port

  -- Start Listening
  listen sock
