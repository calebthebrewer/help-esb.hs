{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import EsbClient

-- ESB Environment
host = "127.0.0.1"
port = 8900

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
  sock <- esbInit "haskell-test" [ "jkl" ] host port

  -- Start Listening
  listen sock
