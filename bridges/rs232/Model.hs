module Main where

import Data.Word
import Data.Bits
import Data.Char (ord)

data Level = Space | Mark
	deriving (Eq, Ord, Show, Read)

data Parity = Even | Odd

data Config = Config
	{ startSize   :: Float		-- 1
	, stopSize    :: Float		-- 1, 1.5, or 2
	, parityStyle :: Maybe Parity	-- optional parity
	, baudRate    :: Integer
	}

-- generate signal for set of bits
writeRS232 :: Config -> [Bool] -> [(Level,Float)]
writeRS232 c bits = 
	[ (Space,bitWidth * startSize c) ] ++
	[ (if bit then Mark else Space,bitWidth) | bit <- allBits ] ++ 
	[ (Mark,bitWidth * startSize c) ]
  where
	bitWidth = 1 / fromIntegral (baudRate c)
	allBits = bits ++ case parityStyle c of
			    Nothing -> []
			    Just Even -> [odd parity]
			    Just Odd  -> [even parity]
	parity = length [ () | True <- bits ]

writeWord8 :: Config -> Word8 -> [(Level,Float)]
writeWord8 c wd = writeRS232 c [ testBit wd i | i <- [0..7]]

send :: Config -> [Word8] -> [(Level,Float)]
send c wds = (Mark,bitWidth) : concatMap (writeWord8 c) wds
  where
	bitWidth = 1 / fromIntegral (baudRate c)	

------------------------------------------------------------

transmit :: [(Level,Float)] -> (Float -> Level)
transmit [] _ 		= Mark	-- idle
transmit ((lvl,w):xs) t 
	| t < w 	= lvl
	| otherwise 	= transmit xs (t - w)


-- drift 0.99 
drift :: Float -> (Float -> Level) -> (Float -> Level)
drift n fn = \ f -> fn (f * n)

------------------------------------------------------------

type Timed a = Float -> (a,Float)

readRS232 :: Config -> (Float -> Level) -> Timed (Maybe [Bool])
readRS232 c inp tm = waitForIdle tm
   where
	waitForIdle tm =
		case inp tm of
		  Space -> waitForIdle (tm + bitWidth / 16)
		  Mark  -> waitForStart tm
		
	waitForStart tm =
		case inp tm of
		  Mark  -> waitForStart (tm + bitWidth / 16)
		  Space -> sample tm
	
		-- + 1.5 ==> Start bit + 1/2 stop bit
		-- + 0.5 ==> middle of bit to be sampled

	sample tm = sanityCheck (tm + bitWidth * (bitCount + 1.5))
		    [ inp (tm + bitWidth * (i + 0.5))
		    | i <- [0..(bitCount + 1)]
		    ]
	
	sanityCheck tm bits
		| head bits == Mark  	= (Nothing,tm)	-- bad start bit
		| last bits == Space 	= (Nothing,tm)	-- bad end bit
		| otherwise 		= (Just binaryBits,tm)
	   where
		binaryBits = map ((==) Mark) $ tail (init bits)

	bitCount = 8 + case parityStyle c of
			 Nothing -> 0
			 Just {} -> 1	-- parity bit

	bitWidth = 1 / fromIntegral (baudRate c)

parityChecker :: Config -> [Bool] -> Maybe Word8
parityChecker c bits =
	case parityStyle c of
	  Just Even | even count -> return $ res $ init bits
	  Just Odd  | odd  count -> return $ res $ init bits
	  Nothing 		 -> return $ res bits
	  _       -> fail "parity failure"
  where
	count  = length [ () | True <- bits ]
	res bs = sum [ b | (b,True) <- zip (iterate (*2) 1) bs ]


receive :: Config -> (Float -> Level) -> [Maybe Word8]
receive c inp = map check (loop 0)
   where 
	loop tm = case readRS232 c inp tm of
		     (r,tm') -> r : loop tm'
	check Nothing = Nothing
	check (Just bs) = parityChecker config bs

------------------------------------------------------------

config = Config 
	{ startSize 	= 1
	, stopSize	= 2
	, parityStyle	= Just Even
	, baudRate	= 9600
	}

------------------------------------------------------------

test = receive config  $ drift 1.04 $ transmit $ send config [0..255]

