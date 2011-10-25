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
          [dest]             -> loopback dest $ \ c -> [c]
          ["--toupper",dest] -> loopback dest $ \ c -> [toUpper c]
          otherwise -> error "usage: lb_loopback [--toupper] loopback-socket"

loopback :: String -> (Char -> String) -> IO ()
loopback dest reflect = openServer dest $ \ hd -> do
        hSetBuffering hd NoBuffering
        forever $ do
                ch <- hGetChar hd
                hPutStr hd (reflect ch)

