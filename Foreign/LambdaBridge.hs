{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.LambdaBridge where

import Foreign.C -- get the C types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.IO.Handle.FD
import System.IO (Handle)	

-- #include <lb_board_connect.h>

board_connect :: [String] -> IO ([Handle],[Handle])
board_connect argv = do
	print argv
	let args = "haskell-lambda-bridge" : argv

	argv_ptrs <- sequence
		[ newCString arg
		| arg <- args
		]

	print argv_ptrs
	argv_ptr <- newArray0 nullPtr argv_ptrs
	print argv_ptr

	hds_ptr <- mallocArray 2

	res <- lb_board_connect (fromIntegral $ length args)
				 argv_ptr
				 1
				 1
				 hds_ptr
				 
	print res

	hds <- sequence [ do i <- peekElemOff hds_ptr i
	 		     fdToHandle i 
	                | i <- take 2 [0..]]

	print hds
	

	return ([hds !! 0],[hds !! 1])

-- // int lb_board_connect(int argc,char **argv,int *hds);

foreign import ccall "lb_board_connect.h lb_board_connect" lb_board_connect 
	:: CInt -> Ptr (Ptr CChar) -> CInt -> CInt -> Ptr CInt -> IO CInt
