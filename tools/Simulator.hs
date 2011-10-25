-- This is the simulator for lambda-bridge.
-- It emulates the different forms of low-level sockets.
-- lb_simulator byte|packet  imported-socket exported-socket

module Main where

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ

main :: IO ()
main = do
        args <- getArgs
        case args of
          ["packet",src,dest] -> packetSim src dest
          ["byte",src,dest] -> byteSim src dest
          otherwise -> error "usage: lb_simulator byte|packet imported-socket exported-socket"

packetSim :: String -> String -> IO ()
packetSim src dest = openServer dest $ \ destH -> do
--        destH <- openSocket dest
        hPutStrLn destH "Hello,1 World"
        print (src,destH)

{-
byteSim :: String -> String -> IO ()
byteSim src dest = do
        srcH <- openClient src
        srcB <- openByteBridge srcH
        frameB <- frameProtocol srcB
        let limit = boundLimit 1
        let pktSize = 100
        sender <- sendWithARQ frameB limit
        recver <- recvWithARQ frameB
        openServer dest $ \ destH -> do
                hSetBuffering destH NoBuffering
                forkIO $ forever $ do 
                        bs <- B.hGetSome destH pktSize
                        print bs
                        sender bs
                        print "send str"
                forkIO $ forever $ do 
                        bs <- recver 
                        print ("recv",bs)
                        B.hPut destH bs
                return ()
-}

byteSim :: String -> String -> IO ()
byteSim src dest = do
        srcH <- openClient src
        hSetBuffering srcH NoBuffering
        openServer dest $ \ destH -> do
                destB <- openByteBridge destH
                frameB <- frameProtocol destB
                let limit = boundLimit 1
                let pktSize = 100
                sender <- sendWithARQ frameB limit
                recver <- recvWithARQ frameB

                forkIO $ forever $ do 
                        bs <- B.hGetSome srcH pktSize
                        print bs
                        sender bs
                        print "send str"
                forkIO $ forever $ do 
                        bs <- recver 
                        print ("recv",bs)
                        B.hPut srcH bs
                return ()



