module Main where

import Foreign.LambdaBridge

main = do
	print "Hello"
	(sends,recvs) <- board_connect []
	print "Done"
