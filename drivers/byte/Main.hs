
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
          [src,dest] -> lb_byte src dest
          otherwise -> error "usage: lb_byte imported-socket exported-socket"

lb_byte :: String -> String -> IO ()
lb_byte src dest = do
        srcH <- openAsClient src
        srcB <- openByteBridge srcH
        frameB <- frameProtocol srcB
        let limit = boundLimit 1
        let pktSize = 100
        sender <- sendWithARQ frameB limit
        recver <- recvWithARQ frameB
        openAsServer dest $ \ destH -> do
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

