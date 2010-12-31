
-- | Support for providing a Frame-based API on top of an unreliable bytestream.

module Foreign.LambdaBridge.Frame
	( crc
	, maxFrameSize
	, frameProtocol
	) where

import Data.Word
import Data.Bits
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS

import Foreign.LambdaBridge.Bridge

import Numeric

{-

Frame format:

<pre>
  <- sync + size            ->|<- payload ...              ->|
  +------+------+------+------+----------------+------+------+
  | 0xf1 |  sz  |   CRC-16    |  ... DATA .... |   CRC-16    |
  +------+------+------+------+----------------+------+------+
</pre>

The '0xe4' is the sync marker starter; then comes the size (1 ...217),
then the CRC-16 for the [0xf1,sz]. This CRC-16 will never contain 0xf1
in either byte (causing the upto 238 restriction). This means that
when looking for a header, if a 0xe4 is found in the sz or CRC positions,
this marks a candidate for a new sync marker, and the one being
processed in corrupt.

-}

-- | Compute the crc for a sequence of bytes, assuming LSB processing.
-- The arguments are the crc code, and the initial value, often -1.
--
--For example, CRC-16-CCITT, LSB first (for use with RS-232), is crc 0x1021 0xffff.

crc :: (Bits w) => w -> w -> [Word8] -> w
crc code start bs = foldl (\ crc b -> 
			     let b' = if b then 1 else 0
				 crc' = (crc `shiftL` 1) `xor` b'
			     in if crc `testBit` (bitSize crc - 1)
				then crc' `xor` code
			        else crc'
		 ) start
		 $ concat [  [ b `testBit` i
			    | i <- reverse [0..7]
			    ] 
			  | b <- bs
			  ]
--   where
--	code = 0x1021 -- 0x8408

-- | The maximum frame payload size, not including CRC.
maxFrameSize :: Int
maxFrameSize = 238

-- This attempts all possible headers, and checks to see if there are any 
-- 0xf1's (the tag) appearing in the size or checksum.

sanityCheck = 
	[
	       let v = crc (0x1021 :: Word16) 0xffff [tag,sz,0,0]
	           b = fromIntegral (v `div` 256) :: Word8
	           a = fromIntegral v :: Word8
 	           v' = crc (0x1021 :: Word16) 0xffff [tag,sz,b,a]
               in if a == tag || b == tag || sz == tag || v' /= 0
	       then show (showH b,showH a,showH v',sz)
	       else "-" -- good outcome

	 | sz <- [0..fromIntegral maxFrameSize]
	 ]
  where
	tag :: Word8
	tag = 0xf1

-----------------------------------------------------------------------
-- | 'frameProtocol' provides a Bridge Frame from the services of a 'Bridge Byte'. (stub, for now.)
-- It is thread safe (two Frame writes to the same bridge will not garble each other)

frameProtocol :: Bridge () Byte -> IO (Bridge () Frame)
frameProtocol byte_bridge = do
	sending <- newEmptyMVar

	let write wd = toBridge byte_bridge () (Byte wd)
	let read = do Byte wd <- fromBridge byte_bridge ()
		      return wd
	
	let writeWithCRC xs = do
		sequence_ [ write x | x <- xs ]
		let crc_val = crc (0x1021 :: Word16) 0xffff (xs ++ [0,0])
		write (fromIntegral (crc_val `div` 256))
		write (fromIntegral (crc_val `mod` 256))

--	let checkCRC xs = do
		


	let sender = do
		bs <- takeMVar sending
		let hdr :: [Word8]
		    hdr = [0xf1,fromIntegral $ BS.length bs]
		writeWithCRC hdr
		writeWithCRC (BS.unpack bs)
		sender
		
	forkIO $ sender
{-
	let findHeader :: IO ()
	    findHeader = do
		wd <- read
		if wd == 
-}
{-
	forkIO $ fromBridge byte_bridge $ \ a -> do
--		print a
		return True
-}

	let recvFrame = return undefined

	return $ Bridge 
		{ toBridge = \ () (Frame bs) ->
			if BS.length bs > maxFrameSize
			then fail "packet exceeded max frame size"
			else putMVar sending bs
		, fromBridge = \ () -> recvFrame
		}



-----------------------------------------------------------------------


-- example1: CRC-CCITT (0x1D0F)	0xE5CC
-- starts with 0xffff, 0x1D0F is output if no input
-- or appending 

example1 = [49..57]
example1_crc = crc (0x1021 :: Word16) 0xffff (example1 ++ [0,0])

example2 = example1 ++ [0xe5,0xcc]
example2_crc = crc (0x1021 :: Word16) 0xffff example2

showH x = "0x" ++ showHex x ""


main :: IO ()
main = do
	bridge_byte <- nullBridge
	
	forkIO $ let loop = do
			_ <- fromBridge bridge_byte ()
			loop
		 in loop

	bridge_byte' <- debugBridge (\ (Byte w) -> showH w) bridge_byte

--	sequence_ [ toBridge bridge_byte' x | x <- [0..255]]

	bridge_frame <- frameProtocol bridge_byte'

	toBridge bridge_frame () $ Frame (BS.pack [49..57])

	return ()