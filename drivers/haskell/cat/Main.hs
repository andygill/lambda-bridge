module Main where
	
import System.IO
import Control.Monad
import Network.LambdaBridge.Driver

-- Trivial driver, just reflects back the input. Assumes one in, one out.
main = bundle_driver "cat" $ \ args ins outs ->
	case (args,ins,outs) of
	  (opt,[h1],[h2]) -> forever $ do
		c <- hGetChar h1
		hPutChar h2 c
