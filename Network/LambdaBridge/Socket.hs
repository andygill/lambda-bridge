-- | This modules offers a trivial rendering of Sockets.

module Network.LambdaBridge.Socket where

import System.IO
import Network
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char

import Network.LambdaBridge.Bridge


-- | 'openClient' opens a (remote or local) socket, and returns the UNIX handle for the socket.
openClient :: String -> IO Handle
openClient nm = connectTo "localhost" $ UnixSocket nm


-- | 'openServer' creates a socket, listens to the socket, 
-- and calls a callback when the socket is connected to.
-- The callback may be invoked multiple times.
-- 'openServer' does not start listening for new connections until the callback from
-- a specific connection has returned. 
-- The call to openServer never terminates.

openServer :: String -> (Handle -> IO ()) -> IO ()
openServer nm callback =
        finally (do sock <- listenOn $ UnixSocket nm
                    forever $ do 
                       print "accepting"
                       (hd,_host,_port) <- accept sock
                       print "acceped"
                       callback hd)
                (removeFile nm)

-- | Turn a (Socket) 'Handle' into a Bridge (of) Byte
openByteBridge :: Handle -> IO (Bridge Byte)
openByteBridge hd = do
        hSetBuffering hd NoBuffering
        let reading = do ch <- hGetChar hd
                         return (Byte (fromIntegral (ord ch)))
            writing (Byte x) = hPutChar hd (chr (fromIntegral x))
        return $ Bridge writing reading

        
