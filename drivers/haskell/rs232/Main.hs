module Main where
	
import System.IO
import Data.Char (toUpper)
import Data.Bits
--import Network.Socket
import Network.BSD
import Data.List
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Char as Char
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import Control.Concurrent.MVar
import System.Timeout 
import System.Random

import Prelude hiding (getContents)

import Network.LambdaBridge.Driver
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout

-- This Bridge service uses UDP to send and recieve requests,
-- over (default) port 9237.

-- Remember:
--   % nc -u -l 9237 | mod


data SessionHandle = SessionHandle Socket SockAddr


-- On linux, we need to give permissions to read/write the TTY,
-- and 
--   % stty -F /dev/ttyS0


main = bridge_byte_driver "lb_rs232" (boundLimit 1) $ \ args -> do
   hPutStrLn stderr ("Remote Service:" ++ show args)
   case args of
     [name,tty,speed] -> do
             

             print (name,tty,speed)
             return $ undefined
{-
	case args of
	  (remote_cmd:hostname:port:style:rest) -> do
		print $ "got" ++ show (hostname,port,rest)

		let sockType = case style of
			  "UDP" -> Datagram
			  "TCP" -> Stream
			  _     -> error "is this UDP?"

 		addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    (Just hostname) (Just port)
		let serveraddr = head addrinfos

		sock <- socket AF_INET sockType defaultProtocol

	 	connect sock $ addrAddress serveraddr

		return $ Bridge
			{ toBridge = \ (Frame bs) -> sendAll sock bs
			, fromBridge = do bs <- recv sock 2048
					  return $ Frame bs
			}

-}