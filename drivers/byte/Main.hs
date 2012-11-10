
module Main where

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Driver
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ
import Network.LambdaBridge.Logging

main :: IO ()
main = do
        init_logging
        args <- getArgs
        case args of
          [src,dest] -> lb_byte src dest
          otherwise -> error "usage: lb_byte imported-socket exported-socket"

lb_byte :: String -> String -> IO ()
lb_byte src dest = do
        srcH <- openAsClient src
        srcB <- openByteBridge srcH
        byte_driver srcB 1 254 dest
