
module Main where

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B
import Data.Default
import System.Random

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ

-- This is where most of the testing will go on.

main :: IO ()
main = do
        args <- getArgs
        case args of
          [lhs,rhs] -> buildBridge lhs rhs
          otherwise -> error "usage: lb_bridge lhs-socket rhs-socket"

buildBridge :: String -> String -> IO ()
buildBridge lhs rhs = do
        lhsV <- newEmptyMVar
        rhsV <- newEmptyMVar
        forkIO $ openOnceAsServer lhs >>= putMVar lhsV
        forkIO $ openOnceAsServer rhs >>= putMVar rhsV

        hd1 <- takeMVar lhsV
        hd2 <- takeMVar rhsV

        hSetBuffering hd1 NoBuffering
        hSetBuffering hd2 NoBuffering

        forkIO $ forever $ do
                bs <- B.hGetSome hd1 4096
                B.hPut hd2 bs

        forever $ do
                bs <- B.hGetSome hd2 4096
                B.hPut hd1 bs

        

        bridge1 <- openByteBridge hd1
        bridge2 <- openByteBridge hd2

        let real = def { loseU = 0.0
                       , mangleU = 0.1
                       , mangler = \ r (Byte a) -> Byte (floor (r * 256) + a) 
			}

        connectBridges bridge1 def def bridge2

        return ()
