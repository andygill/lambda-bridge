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

-- On linux, we need to give permissions to read/write the TTY,
-- and 
--   $ chmod a+rw /dev/ttyS0
--   $ stty -F /dev/ttyS0
--   $ stty -F /dev/ttyS0 raw -echo -parity -istrip -parenb cstopb 115200

main = bridge_byte_driver "lb_rs232" (boundLimit 1) $ \ args -> do
   hPutStrLn stderr ("Remote Service:" ++ show args)
   case args of
     [name,tty,speed] -> do

             print (name,tty,speed)

             -- set up the socket / serial / etc

             let sendChar = undefined 
                 readChar = undefined

             let writeWord8 :: Word8 -> IO ()
                 writeWord8 = sendChar . chr . fromIntegral

                 readWord8 :: IO Word8
                 readWord8 = do
                        c <- readChar 
                        return $ fromIntegral $ ord $ c

             return $ Bridge
		{ toBridge = \ (Byte b) -> do
                        writeWord8 b
		, fromBridge = do 
		        b <- readWord8
			return $ Byte b
		}
