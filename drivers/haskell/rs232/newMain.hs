module Main where
	
import System.IO
import Control.Monad
import Network.LambdaBridge.Driver
import Control.Concurrent
import System.Hardware.Serialport   -- Need to install: "cabal install serialport"
import Data.Maybe
--import System.Posix.Process

port = "/dev/ttyS0"
portSettings = SerialPortSettings { commSpeed   = CS9600,
                                    bitsPerWord = 8,
                                    stopb       = One,
                                    parity      = NoParity,
                                    flowControl = NoFlowControl,
                                    timeout     = 0 }
useDTR = False
useRTS = False

--main :: IO ([ThreadId], [ThreadId])
main = do serialPortHandle <- openSerial port defaultSerialSettings
          setDTR serialPortHandle useDTR
          setRTS serialPortHandle useRTS
          bundle_driver "rs232" $ createHandleThreads serialPortHandle
--          closeSerial serialPortHandle
          
createHandleThreads :: SerialPort->[String]->[Handle]->[Handle]-> IO ()
createHandleThreads serialPortHandle args ins outs = do
    sequence $ map (\ x -> forkIO $ rs232recv serialPortHandle x) ins
    sequence $ map (\ x -> forkIO $ rs232send serialPortHandle x) outs
    return ()


{-
createHandleThreads :: [String]->[Handle]->[Handle]-> IO
createHandleThreads args ins outs = do let createRecvThreads = map (\ x -> forkIO $ rs232recv x) ins
                                       let createSendThreads = map (\ x -> forkIO $ rs232send x) outs
                                       threads <- sequence $ unpair $ zip createRecvThreads createSendThreads
                                       let pairedThreads = unzip $ pair threads
                                       return pairedThreads
-}

rs232recv :: SerialPort->Handle -> IO ()
rs232recv serialPortHandle h = forever $ do 
    c <- recvChar serialPortHandle
    if (isJust c) then (hPutChar h $ fromJust c) else return ()

rs232send :: SerialPort->Handle -> IO ()
rs232send serialPortHandle h = forever $ do
    c <- hGetChar h
    sendChar serialPortHandle c

pair :: [a] -> [(a,a)]
pair (x:y:zs) = (x,y):(pair zs)
pair _       =  []

unpair :: [(a,a)] -> [a]
unpair = foldr (\ x y -> (fst x):(snd x):y) []

{-bundle_driver "cat" $ \ args ins outs ->
	case (args,ins,outs) of
	  (opt,[h1],[h2]) -> forever $ do
		c <- hGetChar h1
		hPutChar h2 c
-}

