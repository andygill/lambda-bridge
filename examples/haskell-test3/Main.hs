{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Foreign.LambdaBridge
import System.IO
import Control.Concurrent

host = "rome"
--host = "127.0.0.1" --- "localhost", "127.0.0.1", -

main = do
	putStrLn "Connecting to a remote lambda bridge"
	(send,recv) <- simple_board_connect ["./dist/build/lb_udp/lb_udp", host, "9237", "UDP"]

	let loop n = do
		hPutStrLn send $ take 900 $ (cycle $"Hello, World!: " ++ show n)
		hFlush send
		str <- hGetLine recv
--		putStrLn str
		if (n `mod` 1 == 0) then 
			putStrLn str else return ()
--		threadDelay 100
		loop (n + 1)
	loop 0

	putStrLn "Exiting lambda bridge"

