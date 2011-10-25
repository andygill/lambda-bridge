module Main where
	
import System.IO
import Control.Monad

-- import Network.LambdaBridge.Sockets

-- Trivial driver, just reflects back the input. Assumes one in, one out.
main = error ""
{-
main = openServer "
        bundle_driver "cat" $ \ args ins outs ->
	case (args,ins,outs) of
	  (opt,[h1],[h2]) -> forever $ do
		c <- hGetChar h1
		hPutChar h2 c
-}