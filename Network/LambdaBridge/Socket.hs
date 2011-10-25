-- | This modules offers a trivial rendering of Sockets.

module Network.LambdaBridge.Socket where

import System.IO
import Network
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char

import Network.LambdaBridge.Bridge

-- 'SocketName' is the name of a UNIX socket,
-- or a number (for a TCP/IP port ID).
--
-- When using make a remote connection, a colon can be used: "drumchapel:1234"

type SocketName = String

-- | 'openAsClient' opens a (remote or local) socket, and returns the UNIX handle for the socket.
openAsClient :: SocketName -> IO Handle
openAsClient nm = connectTo "localhost" $ UnixSocket nm


-- | 'openAsServer' creates a socket, listens to the socket, 
-- and calls a callback when the socket is connected to.
-- The callback may be invoked multiple times.
-- 'openAsServer' does not start listening for new connections until the callback from
-- a specific connection has returned. 
-- The call to openAsServer never terminates.

openAsServer :: SocketName -> (Handle -> IO ()) -> IO ()
openAsServer nm callback =
        finally (do sock <- listenOn $ UnixSocket nm
                    forever $ do 
                       print "accepting"
                       (hd,_host,_port) <- accept sock
                       print "acceped"
                       callback hd)
                (removeFile nm)

-- | 'openOnceAsServer' is a version of openAsServer than only opens the socket
-- for a single connection, after waiting for the connection.

openOnceAsServer :: SocketName -> IO Handle
openOnceAsServer nm = do
        finally (do sock <- listenOn $ UnixSocket nm
                    print "accepting"
                    (hd,_host,_port) <- accept sock
                    print "acceped"
                    return hd)
                (removeFile nm)

-- | Turn a (Socket) 'Handle' into a Bridge (of) Byte
openByteBridge :: Handle -> IO (Bridge Byte)
openByteBridge hd = do
        hSetBuffering hd NoBuffering
        let reading = do ch <- hGetChar hd
                         return (Byte (fromIntegral (ord ch)))
            writing (Byte x) = hPutChar hd (chr (fromIntegral x))
        return $ Bridge writing reading
