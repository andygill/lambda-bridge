
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

        bridge1 <- openByteBridge hd1
        bridge2 <- openByteBridge hd2

        let real = def { loseU = 0.0
                       , mangleU = 0.1
                       , mangler = \ r (Byte a) -> Byte (floor (r * 256) + a) 
			}

        connectBridges bridge1 real real bridge2

        return ()
