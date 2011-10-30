module Network.LambdaBridge.Driver where

import System.Environment
import GHC.IO.Handle.FD
import Control.Concurrent
import System.IO 
import Control.Monad
import Data.Char(isDigit)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Timeout
import Network.LambdaBridge.Multiplex
import Network.LambdaBridge.Frame
import Network.LambdaBridge.ARQ
import Network.LambdaBridge.Socket


-- | 'byte_driver' constructs a socket on top of a given 'Byte' bridge.

byte_driver :: Bridge Byte      -- ^ the ability to send and receive bytes downstream
            -> Float            -- ^ The MAX timeout size, in seconds.
            -> Int              -- ^ The MAX packet size.
            -> SocketName       -- ^ The name of the socket we make for this driver
            -> IO ()            -- ^ never returns.
byte_driver bridge maxTime maxPacket socketName = do
        frameB <- frameProtocol bridge
	frame_driver frameB maxTime maxPacket socketName

-- | 'frame_driver' constructs a socket on top of a given 'Frame' bridge.

frame_driver :: Bridge Frame     -- ^ the ability to send and receive frames/packets downstream
             -> Float            -- ^ The MAX timeout size, in seconds.
             -> Int              -- ^ The MAX packet size.
             -> SocketName       -- ^ The name of the socket we make for this driver
             -> IO ()            -- ^ never returns.
frame_driver frameB maxTime maxPacket socketName = do
        let limit = boundLimit maxTime
        arqB <- arqProtocol frameB limit
        openAsServer socketName $ \ destH -> do
                hSetBuffering destH NoBuffering
                hSetBinaryMode destH True     -- I think this is done anyway
                forkIO $ forever $ do 
                        bs <- BS.hGetSome destH maxPacket
                        toBridge arqB bs
                forkIO $ forever $ do 
                        bs <- fromBridge arqB
--                        threadDelay (100 * 1000)      throttle, please
                        BS.hPut destH bs
                return ()
