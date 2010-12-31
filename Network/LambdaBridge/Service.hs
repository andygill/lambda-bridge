module Network.LambdaBridge.Service (bridge_service) where

import System.Environment
import GHC.IO.Handle.FD
import System.IO 
import Data.Char(isDigit)

-- | 'bridge_server' builds a service, which takes arguments and Handles,
-- and runs something, typically talking to a board, or virtual hardware.

bridge_service :: ([String] -> [Handle] -> [Handle] -> IO ()) -> IO ()
bridge_service cont = do
	print "bridge_service"
	args <- getArgs
	case args of
	  ("1":n:"1":m:rest) | all isDigit n && all isDigit m -> do
		send <- fdToHandle (read n)
		resv <- fdToHandle (read m)
		hSetBuffering send NoBuffering
		hSetBuffering resv NoBuffering
		cont rest [send] [resv]

	  _ -> error $ "bad (or unsupported) argument format for service" ++ show args
	
