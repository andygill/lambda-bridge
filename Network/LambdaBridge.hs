{-# LANGUAGE ForeignFunctionInterface #-}

module Network.LambdaBridge (connect, SocketName) where

import System.IO
import Network.LambdaBridge.Socket

-- | 'connect' connects to a FPGA, via a pre-initialized lambda-bridge driver.
-- This is the sole user-facing side of lambda-bridge, all other
-- libraries are for engineering building drivers for lambda-bridge.
--
-- Note that connect is a (almost trivial) alias of connectTo from the network library.
-- There is no reason why, if prefered, the standard network libary,
-- or another socket opening library can not be used instead to open the socket.
-- It is provided for completeness and convenience.

connect :: SocketName -> IO Handle
connect = openAsClient
