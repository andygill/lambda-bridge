module Foreign.LambdaBridge where

import System.IO (Handle)	

board_connect :: [String] -> IO ([Handle],[Handle])
board_connect argv = do
	print argv

	return ([],[])
