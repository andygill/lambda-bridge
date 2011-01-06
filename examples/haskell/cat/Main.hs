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

main = do
	putStrLn "Connecting to 'cat' lambda bridge"
	([send],[recv]) <- board_connect (1,1) ["lb_cat","--debug"] 


	let loop n = do
		hPutStrLn send $ "Message " ++ show n ++ "!"
		hFlush send
		loop (n+1)

	forkIO $ loop 0

	forever $ do 
		str <- hGetLine recv
		putStrLn str

	putStrLn "Exiting lambda bridge"

