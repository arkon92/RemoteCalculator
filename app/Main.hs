module Main where

import Network (listenOn, PortID(PortNumber))
import Server

main :: IO ()
main = do
  putStrLn "Server initiatization"
  socket <- listenOn $ PortNumber 9000
  putStrLn $ "Server started on port " ++ show 9000
  handleNewConnection socket