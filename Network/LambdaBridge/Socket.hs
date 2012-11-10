module Network.LambdaBridge.Socket (socketBytesBridge) where

import Network
import Network.LambdaBridge.Bridge

import System.IO
import Data.Char
import System.Directory
import Control.Exception
import Control.Monad
import Data.Char
import System.IO.Unsafe
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString


-- | 'SocketName' is the name of a socket.
--
-- When using to name a remote connection, a colon can be used to denote remoteness: "drumchapel:1234"
--
-- Otherwise, a named *socket* is intended.
--

type SocketName = String

-- open a bridge of *bytes* to the given socket name. This depends on the
-- remote service being configured for bytes. (for example, SOCK_STREAM)

socketBytesBridge :: HostName -> PortID -> IO (Bridge Bytes)
socketBytesBridge hostName sockName = do
        hd <- connectTo hostName sockName
        hSetBuffering hd NoBuffering            -- for now
        return $ Bridge { toBridge = \ (Bytes bs) -> do
                                BS.hPut hd bs
                        , fromBridge = do
                                bs <- BS.hGet hd 1
                                if BS.length bs == 0 then fail "socket closed"
                                                     else return $ Bytes bs
                        }

-- open a bridge of *frames* to the given socket name. This depends on the
-- remote service being configured for frames (for example, SOCK_DGRAM).

udpFrameBridge :: SocketName -> IO (Bridge Frame)
udpFrameBridge = error "udpFrameBridge: unsupported (yet)"

