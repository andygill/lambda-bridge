
module Main where

import System.IO
import Control.Concurrent
import System.Environment
import Control.Monad
import qualified Data.ByteString as B
import System.Hardware.Serialport
import Data.Word
import Data.Maybe
import Data.Char

import Network.LambdaBridge.Socket 
import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Frame
import Network.LambdaBridge.Driver
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.ARQ

-- On linux, we need to give permissions to read/write the TTY,
-- and 
--   $ chmod a+rw /dev/ttyS0
--   $ stty -F /dev/ttyS0
--   $ stty -F /dev/ttyS0 raw -echo -parity -istrip -parenb cstopb 115200

main :: IO ()
main = do
        args <- getArgs
        case args of
          [src,dest] -> lb_rs232 src dest
          otherwise -> error "usage: lb_rs232 /dev/tty-name exported-socket"

portSettings = SerialPortSettings { System.Hardware.Serialport.commSpeed   = CS115200,
                                    System.Hardware.Serialport.bitsPerWord = 8,
                                    System.Hardware.Serialport.stopb       = One,
                                    System.Hardware.Serialport.parity      = NoParity,
                                    System.Hardware.Serialport.flowControl = NoFlowControl,
                                    System.Hardware.Serialport.timeout     = 10
				  }
				    					
useDTR = False
useRTS = False

lb_rs232 :: String -> String -> IO ()
lb_rs232 src dest = do
        serialPortHandle <- openSerial src portSettings
        setDTR serialPortHandle False
        setRTS serialPortHandle False

        let writeWord8 :: Word8 -> IO ()
            writeWord8 = sendChar serialPortHandle . chr . fromIntegral

            readWord8 :: IO Word8
            readWord8 = do
                   c <- recvChar serialPortHandle
                   if (isJust c) then (return $ fromIntegral $ ord $ fromJust c) else readWord8

            srcB = Bridge
		{ toBridge = \ (Byte b) -> do
                        writeWord8 b
		, fromBridge = do 
		        b <- readWord8
			return $ Byte b
		}

        byte_driver srcB 10 254 dest
