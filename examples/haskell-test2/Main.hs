{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Foreign.LambdaBridge
import System.IO

main = do
	putStrLn "Connecting to 'a.out' lambda bridge"
	([send],[recv]) <- board_connect (1,1) ["haskell-test2-bridge"]

	hPutStrLn send "Hello, World!"
	hFlush send
	str <- hGetLine recv
	putStrLn str

	putStrLn "Exiting lambda bridge"

