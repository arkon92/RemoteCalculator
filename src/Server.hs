module Server
    (handleNewConnection) where

import Network (accept, Socket)
import System.IO (hGetLine, hPutStrLn, Handle, hClose)
import Control.Concurrent (forkIO)

handleNewConnection :: Socket -> IO ()
handleNewConnection socket = do
  (handle, _, _) <- accept socket
  forkIO $ executeCommand handle
  hPutStrLn handle "Available commands: \n- add\n- sub\n- mul\n- div"
  handleNewConnection socket

executeCommand :: Handle -> IO ()
executeCommand handle = do
    line <- hGetLine handle
    let command = words line
    case command of
          ("exit" : _) -> closeHandle handle
          _ -> handleCommand handle command

echoHelp :: Handle -> [String] -> IO ()
echoHelp handle cmd = do
    hPutStrLn handle "Available commands: \n- add\n- sub\n- mul\n- div"

operation :: (Int -> Int -> Int) -> Handle -> [String] -> IO ()
operation op handle cmd = do
  hPutStrLn handle $ show $ op (read $ (cmd !! 1) :: Int) (read $ (cmd !! 2) :: Int)

handleCommand :: Handle -> [String] -> IO ()
handleCommand handle command = do
  case command of
          [] -> do hPutStrLn handle "Empty command"
          ("help" : a : b :[]) -> echoHelp handle command
          ("add" : a : b :[]) -> operation (+) handle command
          ("sub" : a : b :[]) -> operation (-) handle command
          ("mul" : a : b :[]) -> operation (*) handle command
          ("div" : a : b :[]) -> if (read b :: Int) /= 0
                         then operation (div) handle command
                          else hPutStrLn handle "Can't divide by 0"
          _ -> do hPutStrLn handle "Unknown command - Type help for more information"
  executeCommand handle

closeHandle :: Handle -> IO ()
closeHandle handle = do
  putStrLn "User left"
  hPutStrLn handle "Bye."
  hClose handle