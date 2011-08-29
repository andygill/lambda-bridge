{-# LANGUAGE ForeignFunctionInterface #-}

module Network.LambdaBridge (simple_board_connect, board_connect) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.IO.Handle.FD
import System.IO


-- | 'simple_board_connect' connects to a FPGA, via a specified lambda-bridge driver.

simple_board_connect :: [String] -> IO (Handle,Handle)
simple_board_connect argv = do
	(sends,recvs) <- board_connect (1,1) argv
	case (sends,recvs) of
	  ([s],[r]) -> return (s,r)
	  _ -> error $ "simple_board_connect failed, wrong number of channels opened"

-- | 'board_connect' connects to a FPGA, via a specified lambda-bridge driver,
--  and allows for multiple SEND and RECV channels.

board_connect :: (Int,Int) -> [String] -> IO ([Handle],[Handle])
board_connect (1,1) argv = do
	let args = "haskell-lambda-bridge" : argv
	argv_ptrs <- sequence
		[ newCString arg
		| arg <- args
		]
	argv_ptr <- newArray0 nullPtr argv_ptrs
	hds_ptr <- mallocArray 2
	res <- lb_board_connect (fromIntegral $ length args)
				 argv_ptr
				 1
				 1
				 hds_ptr
	hds <- sequence [ do i <- peekElemOff hds_ptr i
	 		     fdToHandle i 
	                | i <- take 2 [0..]]
	sequence_ [ hSetBuffering hd NoBuffering | hd <- hds ]
	return ([hds !! 0],[hds !! 1])
board_connect _ _ = error $ "board_connect: only single FIFOs in each direction are currently supported"

foreign import ccall unsafe "lb_board_connect.h lb_board_connect" lb_board_connect 
	:: CInt -> Ptr (Ptr CChar) -> CInt -> CInt -> Ptr CInt -> IO CInt
