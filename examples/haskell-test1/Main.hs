-- Example of interacting with a lambda bridge.

module Main where

import Foreign.LambdaBridge
import System.IO

main = do
	putStrLn "Connecting to 'cat' lambda bridge"
	([send],[recv]) <- board_connect ["cat","DEBUG"]

	hPutStrLn send "Hello, World!"
	hFlush send
	str <- hGetLine recv
	putStrLn str

	putStrLn "Exiting lambda bridge"

