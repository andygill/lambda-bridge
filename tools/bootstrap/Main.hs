-- Code to test the lambda bridge.

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B
import Data.Default
import System.Random
import Data.Char
import qualified Data.ByteString as BS
import Data.Time.Clock
import Numeric

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ


main :: IO ()
main = do
        args <- getArgs
        case args of
          [port] -> masterClient port
          ["--slave",port] -> slaveClient port
          otherwise -> error "usage: lb_bootstrap [--slave] socket"

masterClient port = do
        clientH <- openAsClient port
        hSetBuffering clientH NoBuffering
        let send = BS.hPut clientH
            recv n = BS.hGet clientH n
            expect msg 
              | BS.null msg = return ()
              | otherwise   = do
--                    print ("waiting for",msg)
                    b <- recv (BS.length msg)
--                    print (b,BS.length b)
                    if b `BS.isPrefixOf` msg
                     then expect (BS.drop (BS.length b) msg)
                     else fail $ "bad return: " ++ show (msg,b)

        putStrLn "Test 1: Single packet (roundtrip)"
        let msg1 = BS.pack [1..10]

        t0 <- getCurrentTime
        send msg1
        t1 <- getCurrentTime
        expect msg1
        t2 <- getCurrentTime
        putStrLn $ "Test 1: Passed: " 
                ++ show (realToFrac (t1 `diffUTCTime` t0) :: Float) ++ "s to send, "
                ++ show (realToFrac (t2 `diffUTCTime` t1) :: Float) ++ "s to receive, "
                ++ show (realToFrac (t2 `diffUTCTime` t0) :: Float) ++ "s round trip"


        putStrLn $ "Test 2: different sized packets"
        let loop2 n = do
                t0 <- getCurrentTime
                let msg2 = BS.pack [fromIntegral (c :: Integer) | c <- [1..n]]
                let loop m = do
                        send msg2
                        expect msg2
                        t1 <- getCurrentTime
                        let d = realToFrac (t1 `diffUTCTime` t0) :: Float
                        if d < 5 then loop (m+1) else return (d,m)
                (d,m) <- loop 0
                putStrLn $ showFFloat (Just 2) d "" ++ "s, " 
                        ++ show m ++ " packets of "  ++ show n ++ ", "
                        ++ showBPS (fromIntegral (m * n * 2 * 8) / d) 
                if m > 10 && n < 16384 
		     then loop2 (ceiling (fromIntegral n * (if n < 32 then 2.0 else 1.1)))
		     else return ()
        loop2 1
        
        putStrLn "Done"

showBPS :: Float -> String
showBPS n | n > 1000 * 1000 = showFFloat (Just 2) (n / (1000 * 1000)) "Mbps"
          | n > 1000        = showFFloat (Just 2) (n / 1000)          "Kbps"
          | otherwise       = showFFloat (Just 2) n                   "bps"

-- This just relects the incoming stream, as the outgoing stream.
slaveClient port = do
        clientH <- openAsClient port
        hSetBuffering clientH NoBuffering
        forever $ do
                bs <- BS.hGetSome clientH 4096
                BS.hPut clientH bs
