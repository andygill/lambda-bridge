-- | This modules offers a trivial rendering of Sockets.

module Network.LambdaBridge.Socket where

import System.IO
import Network
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Logging

debug = debugM "lambda-bridge.socket"

-- | 'SocketName' is the name of a socket.
--
-- When using to name a remote connection, a colon can be used to denote remoteness: "drumchapel:1234"
--
-- When used to name a TCP/IP port ID, a number can be used: "1234"
--
-- Otherwise, a named socket is intended.

type SocketName = String

-- | 'openAsClient' opens a (remote or local) socket, and returns the UNIX handle for the socket.
openAsClient :: SocketName -> IO Handle
openAsClient sockName = 
   case span (/= ':') sockName of
     (hostName,':':portString) | all isDigit portString -> connectTo hostName 
                                                         $ mkPortNumber portString
     (hostName,':':_) -> error $ "socketName failure; bad hostname or socket number:" ++ show sockName
     _                -> connectTo "localhost" $ UnixSocket sockName                

-- | 'openAsServer' creates a socket, listens to the socket, 
-- and calls a callback when the socket is connected to.
-- The callback may be invoked multiple times.
-- 'openAsServer' does not start listening for new connections until the callback from
-- a specific connection has returned. 
-- The call to openAsServer never terminates.

openAsServer :: SocketName -> (Handle -> IO ()) -> IO ()
openAsServer nm callback =
        finally (do sock <- listenOn portId
                    forever $ do 
                       debug $ "accepting socket " ++ show sock
                       acc@(hd,_host,_port) <- accept sock
                       debug $ "accepted " ++ show acc
                       callback hd)
                (case portId of
                   UnixSocket name -> removeFile nm
                   _ -> return ())
  where
          portId = findServerPortID nm

findServerPortID :: String -> PortID
findServerPortID nm 
  | all isDigit nm  = mkPortNumber nm
  | otherwise       = UnixSocket nm


mkPortNumber :: String -> PortID
mkPortNumber portString = PortNumber (fromIntegral (read portString :: Int))

-- | 'openOnceAsServer' is a version of openAsServer than only opens the socket
-- for a single connection, after waiting for the connection.

openOnceAsServer :: SocketName -> IO Handle
openOnceAsServer nm = do
        finally (do sock <- listenOn $ findServerPortID nm
                    debug $ "accepting socket " ++ show sock
                    acc@(hd,_host,_port) <- accept sock
                    debug $ "accepted " ++ show acc
                    return hd)
                (case portId of
                   UnixSocket name -> removeFile nm
                   _ -> return ())
  where
          portId = findServerPortID nm

-- | Turn a (Socket) 'Handle' into a Bridge (of) Byte
openByteBridge :: Handle -> IO (Bridge Byte)
openByteBridge hd = do
        hSetBuffering hd NoBuffering
        let reading = do ch <- hGetChar hd
                         return (Byte (fromIntegral (ord ch)))
            writing (Byte x) = hPutChar hd (chr (fromIntegral x))
        return $ Bridge writing reading
