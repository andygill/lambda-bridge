-- | This modules offers a trivial rendering of Sockets.

module Network.LambdaBridge.Socket where

import System.IO
import Network
import System.Directory
import Control.Exception
import Control.Monad

-- openClient :: String -> IO Handle

-- Can be called multiple times. 
-- Does not start listening to new connections until the callback has returned. 
-- The call to openServer never terminates.

openServer :: String -> (Handle -> IO ()) -> IO ()
openServer nm callback =
        finally (do sock <- listenOn $ UnixSocket nm
                    forever $ do 
                       (hd,_host,_port) <- accept sock
                       callback hd)
                (removeFile nm)

