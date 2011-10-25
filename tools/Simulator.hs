-- This is the simulator for lambda-bridge.
-- It emulates the different forms of low-level sockets.
-- lb_simulator byte|packet  imported-socket exported-socket

module Main where

import Network.LambdaBridge.Socket 

import System.IO
import System.Environment


main :: IO ()
main = do
        args <- getArgs
        case args of
          ["packet",src,dest] -> packetSim src dest
          otherwise -> error "usage: lb_simulator byte|packet imported-socket exported-socket"

packetSim :: String -> String -> IO ()
packetSim src dest = openServer dest $ \ destH -> do
--        destH <- openSocket dest
        hPutStrLn destH "Hello,1 World"
        print (src,destH)
        


