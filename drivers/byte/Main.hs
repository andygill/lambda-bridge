
module Main where

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ

main :: IO ()
main = do
        args <- getArgs
        case args of
          [src,dest] -> lb_byte src dest
          otherwise -> error "usage: lb_byte imported-socket exported-socket"

byte_driver :: Bridge Byte      -- ^ the ability to send and receive bytes downstream
            -> Float            -- ^ The MAX timeout size, in seconds.
            -> Int              -- ^ The MAX packet size.
            -> String           -- ^ The name of the socket we make for this driver
            -> IO ()            -- ^ never returns.
byte_driver bridge maxTime maxPacket socketName = do
        frameB <- frameProtocol bridge
        let limit = boundLimit maxTime
        sender <- sendWithARQ frameB limit
        recver <- recvWithARQ frameB
        openAsServer socketName $ \ destH -> do
                hSetBuffering destH NoBuffering
                forkIO $ forever $ do 
                        bs <- B.hGetSome destH maxPacket
                        print bs
                        sender bs
                        print "send str"
                forkIO $ forever $ do 
                        bs <- recver 
                        print ("recv",bs)
                        B.hPut destH bs
                return ()



lb_byte :: String -> String -> IO ()
lb_byte src dest = do
        srcH <- openAsClient src
        srcB <- openByteBridge srcH
        byte_driver srcB 10 254 dest
