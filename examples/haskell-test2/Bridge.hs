module Main where
	
import Foreign.LambdaBridge.Service
import System.IO
import Data.Char (toUpper)

main = bridge_service $ \ args [send] [recv] -> do
	print "Starting"
	print args
	print send
	print recv
	loop $ do c <- hGetChar send
		  hPutChar recv (toUpper c)

loop m = do m ; loop m