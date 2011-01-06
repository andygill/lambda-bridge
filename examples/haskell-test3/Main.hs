{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Network.LambdaBridge
import System.IO
import Control.Concurrent
import Control.Monad


--host = "rome"
host = "127.0.0.1" --- "localhost", "127.0.0.1", -

main = do
	putStrLn "Connecting to a remote lambda bridge"
	(send,recv) <- simple_board_connect ["./dist/build/lb_udp/lb_udp", host, "9237", "UDP"]

	let loop n = do
		hPutStrLn send $ "Message " ++ show n ++ "!"
		hFlush send
		loop (n+1)

	forkIO $ loop 0

	forever $ do 
		str <- hGetLine recv
		putStrLn str

	putStrLn "Exiting lambda bridge"

