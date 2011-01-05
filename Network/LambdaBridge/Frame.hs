
-- | Support for providing a Frame-based API on top of an unreliable bytestream.

module Network.LambdaBridge.Frame
	( frameProtocol
	, maxFrameSize
	, crc
	) where

import Data.Word
import Data.Bits
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import System.Timeout 
import Numeric
import Data.Char
import Data.Default


import Network.LambdaBridge.Bridge

import System.Random
import Debug.Trace



-- | Compute the crc for a sequence of bytes.
-- The arguments for 'crc' are the crc code, and the initial value, often -1.
--
--For example, CRC-16-CCITT, which is used for 'Frame', is crc 0x1021 0xffff.

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

-- | The maximum frame payload size, not including CRCs or headers, pre-stuffed.
maxFrameSize :: Int
maxFrameSize = 238

-- This attempts all possible headers, and checks to see if there are any 
-- 0xf1's (the tag) appearing in the size or checksum. Not exported.

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
-- | 'frameProtocol' provides a Bridge Frame Frame from the services of a 'Bridge Byte'.
-- It is thread safe (two Frame writes to the same bridge will not garble each other)

{- |

It uses the following Frame format:


> <- sync + size            ->|<- payload ...              ->|
> +------+------+------+------+----------------+------+------+
> | 0xf1 |  sz  |   CRC-16    |  ... DATA .... |   CRC-16    |
> +------+------+------+------+----------------+------+------+


The '0xf1' is the sync marker starter; then comes the size (0 ...238),
then the CRC-16 for the [0xf1,sz]. This CRC-16 will never contain 0xf1
in either byte (causing the upto 238 restriction). This means that
when looking for a header, if a 0xf1 is found in the sz or CRC positions,
this marks a candidate for a new sync marker, and the one being
processed in corrupt.

Furthermore, the data is byte-stuffed 
(<http://en.wikipedia.org/wiki/Bit_stuffing>)
for 0xf1, so 0xf1 in DATA and the DATA-CRC is represented
using the pair of bytes 0xf1 0xff (which will never occur in a header).
In this way, if a byte is lost, the next header can be found
by scaning for 0xf1 N, where N <= 238.

The two arguments for 'frameProtocol' are the timeout in seconds for
reading character for the 'Bridge Byte', and the 'Bridge Byte' itself.

-}

frameProtocol :: Bridge Byte -> IO (Bridge Frame)
frameProtocol byte_bridge = do
	let tag = 0xf1 :: Word8

	-----------------------------------------------------------------------------
	sending <- newEmptyMVar

	let write wd = toBridge byte_bridge (Byte wd)

	let writeWithCRC stuffing xs = do
		let crc_val = crc (0x1021 :: Word16) 0xffff (xs ++ [0,0])
		sequence_ [ do write x
			       if x == tag && stuffing then write 0xff else return ()
			  | x <- xs ++ [ fromIntegral $ crc_val `div` 256
				       , fromIntegral $ crc_val `mod` 256
				       ]
			 ]


	let sender = do
		bs <- takeMVar sending
		let hdr :: [Word8]
		    hdr = [tag,fromIntegral $ BS.length bs]
		writeWithCRC False hdr
		writeWithCRC True (BS.unpack bs)
		sender
		
	forkIO $ sender

	-----------------------------------------------------------------------------

	recving <- newEmptyMVar

	let read = do Byte wd <- fromBridge byte_bridge
		      return wd

	let checkCRC xs = crc (0x1021 :: Word16) 0xffff xs == 0 

	let findHeader :: IO ()
	    findHeader = do
		wd0 <- read	-- the first byte of a packet can wait as long as you like
		wd1 <- read
		wd2 <- read
		wd3 <- read
		findHeader' wd0 wd1 wd2 wd3
		
	    -- you already have the first byte
	    findHeader' :: Word8 -> Word8 -> Word8 -> Word8 -> IO ()
	    findHeader' wd0 wd1 wd2 wd3 
--		| wd0 == 0xf1 && wd1 == 0 && trace "0xf1" False = undefined
		| wd0 == 0xf1
		  && fromIntegral wd1 <= maxFrameSize 
		  && checkCRC [wd0,wd1,wd2,wd3] = findPayload (fromIntegral wd1)
		| otherwise = do
			wd4 <- read
	    		findHeader' wd1 wd2 wd3 wd4

	    readWithPadding :: IO Word8
	    readWithPadding = do
		wd1 <- read

		if wd1 == tag then 
			do wd2 <- read
			   if wd2 == 0xff then return wd1 else do
				wd3 <- read
				wd4 <- read
				findHeader' wd1 wd2 wd3 wd4
				fail "trampoline (hack to clear stack)"
		    else do
			return wd1

	    findPayload :: Int -> IO ()
	    findPayload sz = do

		-- TODO: consider byte stuffing for 0xf1
		xs <- sequence  [ readWithPadding
		 		| i <- [1..(sz + 2)]
				]
--		print xs
		if checkCRC xs then putMVar recving (Frame (BS.pack (take sz xs)))
			       else return ()
--		print xs
		return ()

	let readBridge = do
		findHeader `catch` \ msg -> do
--					print msg
					return ()
		readBridge


	forkIO $ readBridge

	return $ Bridge 
		{ toBridge = \ (Frame bs) ->
			if BS.length bs > maxFrameSize
			then fail "packet exceeded max frame size"
			else putMVar sending bs
		, fromBridge = takeMVar recving
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
	bridge_byte0 <- loopbackBridge
	
	bridge_byte1 <- debugBridge "bridge_byte" bridge_byte0
	let u = def { loseU = 0.01, dupU = 0.01, mangleU = 0.01, mangler = \ g (Byte a) -> 
									let (a',_) = random g
									in Byte (fromIntegral (a' :: Int) + a) }
	bridge_byte2 <- realisticBridge def def bridge_byte0

--	sequence_ [ toBridge bridge_byte' x | x <- [0..255]]

	bridge_frame <- frameProtocol bridge_byte2

	forkIO $ do
		sequence [ toBridge bridge_frame $ Frame (toStr $ "Frame: " ++ show i ++ " " ++ [' '..'~'] ++ [chr 0xf1])
			 | i <- [1..]
			 ]
		return ()
		
	sequence [ do
		frame <- fromBridge bridge_frame
		print frame
			| _ <- [1..1000]]

	return ()
  where
	toStr :: String -> BS.ByteString
	toStr = BS.pack . map (fromIntegral . ord)
