{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.LambdaBridge (board_connect) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.IO.Handle.FD
import System.IO (Handle)

-- | 'board_connect' connects to a FPGA, via a specified lambda-bridge driver.

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
	return ([hds !! 0],[hds !! 1])
board_connect _ _ = error $ "board_connect: only single FIFOs in each direction are currently supported"

foreign import ccall "lb_board_connect.h lb_board_connect" lb_board_connect 
	:: CInt -> Ptr (Ptr CChar) -> CInt -> CInt -> Ptr CInt -> IO CInt
