module Main where

import System.IO
import Network
import System.Directory
import Control.Monad
import System.Environment
import Data.Char

import Network.LambdaBridge.Socket 

-- Trivial "loopback" driver

main :: IO ()
main = do
        args <- getArgs
        case args of
          [dest]             -> loopback dest $ \ c -> return [c]
          ["--toupper",dest] -> loopback dest $ \ c -> return [toUpper c]
          ["--debug",dest]   -> loopback dest $ \ c -> do putStrLn $ "loopback: " ++ show c ++ " (" ++ show (ord c) ++ ")"
                                                          return [c]
          otherwise -> error "usage: lb_loopback [--toupper|--debug] loopback-socket"

loopback :: String -> (Char -> IO String) -> IO ()
loopback dest reflect = openAsServer dest $ \ hd -> do
        hSetBuffering hd NoBuffering
        forever $ do
                ch <- hGetChar hd
                str <- reflect ch
                hPutStr hd str

