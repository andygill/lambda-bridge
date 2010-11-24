{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Foreign.LambdaBridge
import System.IO
import Control.Concurrent

main = do
	putStrLn "Connecting to a remote lambda bridge"
	(send,recv) <- simple_board_connect ["./dist/build/lb_udp/lb_udp","127.0.0.1","9237"]

	let loop n = do
		hPutStrLn send $ "Hello, World!: " ++ show n
		hFlush send
		str <- hGetLine recv
		putStrLn str
--		threadDelay (1000 * 1000)
		loop (n + 1)
	loop 0

	putStrLn "Exiting lambda bridge"

