{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Network.LambdaBridge
import System.IO

main = do
	putStrLn "Connecting to 'cat' lambda bridge"
	([send],[recv]) <- board_connect (1,1) ["lb_cat","--debug"] 

	hPutStrLn send "Hello, World!"
	hFlush send
	str <- hGetLine recv
	putStrLn str

	putStrLn "Exiting lambda bridge"

