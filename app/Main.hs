module Main where

import Server
import qualified Config

main :: IO ()
main = do
  config <- Config.read
  putStrLn "Starting server"
  Server.startServer config
