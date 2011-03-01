{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types #-}

module Hardware.KansasLava.RS232 (rs232out, rs232in, liftWithUART) where

import Data.Ratio

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix
import Data.Sized.Matrix as M

import Language.KansasLava
import Data.Maybe as Maybe
import Data.Char as Char
import Control.Monad	
import Data.Default
import Data.Word
import Debug.Trace
-- Lava implementation of RS232	

type SAMPLE_RATE = X16

data RS232_TX
	= TX_Idle
	| TX_Send X10
	deriving (Show,Eq,Ord)

isTX_Idle :: (Signal sig) => sig RS232_TX -> sig Bool
isTX_Idle = funMap $ \ tx -> return $ tx == TX_Idle

withTX_Send :: (Signal sig) => sig RS232_TX -> sig (Enabled X10)
withTX_Send = funMap $ \ tx -> return $ case tx of
		TX_Send i -> Just i
		_         -> Nothing


-- Template Haskell would help here.
fromRS232_TX :: RS232_TX -> X11
fromRS232_TX TX_Idle = 0
fromRS232_TX (TX_Send n) = fromIntegral n + 1

toRS232_TX :: X11 -> RS232_TX
toRS232_TX 0 = TX_Idle
toRS232_TX n = TX_Send (fromIntegral n - 1)

instance Rep RS232_TX where
    data X RS232_TX    		= X_RS232_TX (Maybe RS232_TX)
    type W RS232_TX             = X4
    unX (X_RS232_TX v) 		= v
    optX b           		= X_RS232_TX b
    repType Witness     	= repType (Witness :: Witness X11)
    toRep (X_RS232_TX v)	= toRep (optX (fmap fromRS232_TX v))
    fromRep v			= X_RS232_TX (fmap toRS232_TX (unX (fromRep v)))
    showRep Witness 
	    (X_RS232_TX v)	= show v

val = var

(.*&.) :: (Signal sig, Rep a) => sig (Enabled a) -> sig Bool -> sig (Enabled a)
(.*&.) en_a bool = packEnabled (en .&&. bool) a
  where
	(en,a) = unpackEnabled en_a

resize :: (Signal sig, Integral x, Rep x, Num y, Rep y) => sig x -> sig y
resize = funMap $ \ x -> return (fromIntegral x)

findBit :: forall sig . (Signal sig) => (Num (sig X10)) => sig Word8 -> sig X10 -> sig Bool
findBit byte x = (coerce) byte .!. ((unsigned) (x - 1) :: sig X8)

rs232out :: forall clk sig a . (Eq clk, Clock clk, sig a ~ Clocked clk a, clk ~ ()) 
	=> Integer			-- ^ Baud Rate.
	-> Integer			-- ^ Clock rate, in Hz.
        -> I (sig (Enabled Word8))      ()
        -> O (sig Bool)                 (sig Bool)
rs232out baudRate clkRate (inp0,()) = (accept,out)
  where
	-- at the baud rate for transmission
	fastTick :: CSeq clk Bool 
    	fastTick = rate (Witness :: Witness X16) (fromIntegral baudRate / fromIntegral clkRate)

    	(in_en,in_val) 	= unpack inp0

        

    	(accept,out) = runRTL $ do
--		readVal <- newArr (Witness :: Witness X10)
		state  <- newReg (TX_Idle       :: RS232_TX)
		accept <- newReg (False  	:: Bool)
		char   <- newReg (0     	:: Word8)
		output <- newReg (True		:: Bool)	-- RS232, SPACE => high
					
		WHEN fastTick $ CASE
		     [ IF (isTX_Idle (reg state) .&&. in_en) $ do
			state := pureS (TX_Send 0)
			accept := high
			char   := in_val
		     , match (withTX_Send (reg state)) $ \ ix -> do
			CASE [ IF (ix .==. maxBound) $ do
				state  := pureS TX_Idle
			     , OTHERWISE $ do
				state := funMap (\ x -> return (TX_Send (x + 1))) ix
			     ]
			CASE [ IF (ix .==. 0) $ do
				output := low	-- start bit
			     , IF (ix .==. 9) $ do
				output := high	-- stop bit
			     , OTHERWISE $ do
				output := findBit (reg char) ix
			     ]
		     ]
		-- accept a value for 1 cycle only, independent of the fastTick
		WHEN (reg accept) $ do
		   accept := low

		-- We need to use 'var accept', because we need to accept the
		-- the on *this* cycle, not next cycle.
		return (var accept,reg output) -- var state) -- reg output) -- var state)




rs232in :: forall clk sig a . (Eq clk, Clock clk, sig a ~ Clocked clk a) 
	=> Integer			-- ^ Baud Rate.
	-> Integer			-- ^ Clock rate, in Hz.
	-> I (sig Bool)         (sig Bool) 	-- ^ signal input x back edge for FIFO
        -> O ()                 (sig (Enabled Word8))
					-- ^ output
rs232in baudRate clkRate (inp',accept) = ((),out)
  where
	-- 16 times the baud rate for transmission,
	-- so we can spot the start bit's edge.
	fastTick :: CSeq clk Bool 
	fastTick = rate (Witness :: Witness X16) (16 * fromIntegral baudRate / fromIntegral clkRate)
	
	inp :: sig Bool
	inp = 
		id
--		observeAloud "inp" 
			inp'

	findByte :: [sig Bool] -> sig Word8
	findByte xs = coerce (pack (matrix xs :: M.Matrix X8 (sig Bool)) :: sig (M.Matrix X8 Bool))

	out = runRTL $ do
		reading <- newReg False
		theByte <- newArr (Witness :: Witness X16)
		outVal  <- newReg (Nothing :: Enabled Word8)
		ready	<- newReg (False :: Bool)
		counter <- newReg (0 :: U8)

		let lowCounter, highCounter :: sig U4
		    (lowCounter,highCounter) = unpack (coerce (reg counter) :: sig (U4,U4))

		WHEN fastTick $ do
	 		CASE [ IF ((reg reading .==. low) .&&. (inp .==. low)) $ do
				counter := 0
				reading := high
			     , OTHERWISE $ do
				counter := reg counter + 1
			     ]
			
			-- We have a 3 sample average, so we wait an aditional 5
			-- to be in the middle of the 16-times super-sample.
			-- So, 5 is 16 / 2 - 3
			WHEN ((reg reading .==. high) .&&. (lowCounter .==. 7)) $ CASE 
			     [ IF (highCounter .<. 9) $ do
				theByte ((unsigned) highCounter) := inp
			     , IF ((highCounter .==. 9) .&&.
				   (reg (theByte 0) .==. low) .&&.
				   (inp .==. high)
				  ) $ do
				-- This should be the stop bit
				outVal := enabledS
					$ findByte [ reg (theByte (fromIntegral i))
						   | i <- [1..8]
						   ]

--			     , OTHERWISE $ do
--				-- restart; should never happen
--				reading := low
			     ]

		-- If you can accept something, and you have something to ac
		WHEN ((accept .==. high) .&&. (isEnabled (reg outVal))) $ do
			outVal := pureS Nothing
			reading := low

		return $
		 	(var outVal) 
--			observeBothAloud "output" (var outVal) (stdCounter)


-- Idea: I think we can parameterize via a functor (twice) here.

-- We use Seq, not CSeq, because this *must* be the top-level clock anyway.
liftWithUART :: Integer
             -> Integer
             -> (    (I  (Seq (Enabled Word8))           (Seq Bool), other_outs)
                  -> (other_ins, O  (Seq Bool)           (Seq (Enabled Word8)))
                )
             -> I (Seq Bool)              other_outs
             -> O other_ins               (Seq Bool) 
liftWithUART baudRate clkRate dut (inp,otherIn) = (otherOut,out)
   where
           ((),valIn)                   = rs232in baudRate clkRate (inp,inAck)
           (otherOut,(inAck,valOut))    = dut ((valIn,outAck),otherIn)
           (outAck,out)                 = rs232out baudRate clkRate (valOut,())


liftWithUART' :: forall f g . (Frac f, Frac g)
             => Integer
             -> Integer
             -> ( f (I  (Seq (Enabled Word8))           (Seq Bool))
                  -> g (O  (Seq Bool)           (Seq (Enabled Word8)))
                )
             -> f (Seq Bool)
             -> g (Seq Bool)
liftWithUART' baudRate clkRate dut input = output
   where
--           f :: forall a . a -> f a
           Build f inp = frac $ input

           ((),valIn)                   = rs232in baudRate clkRate (inp,inAck)
           Build g (inAck,valOut)       = frac $ dut (f (valIn,outAck))

           (outAck,out)                 = rs232out baudRate clkRate (valOut,())           

           output = g out

-- Turn a FIFO (for example) into a extra parameterized thing, like required by


data IdX a = IdX a

instance Functor IdX where
        fmap f (IdX a) = IdX (f a)

data Build f a = Build (forall a . a -> f a) a

class Functor f => Frac f where 
  frac :: f a -> Build f a

instance (Frac ((,) a)) where
  frac (a,b) = Build (\ b -> (a,b)) b

instance Frac IdX where
  frac (IdX a) = Build IdX a

liftFIFO :: (a -> b) -> IdX a -> IdX b
liftFIFO = fmap

data Ack sig a = Ack (sig Bool) a

instance Frac (Ack sig) where
  frac (Ack ack a) = Build (Ack ack) a

-- A version of Fmap that you get for free with Frac
fracFmap :: Frac f => (a -> b) -> f a -> f b
fracFmap fn fa = f (fn a)
   where
           Build f a = frac fa
           
instance Functor (Ack sig) where fmap = fracFmap

fifo' :: (Clock clk, sig ~ CSeq clk, count ~ (ADD w X1), Size count, Size w, Rep a, Rep count, Rep w, Num count, Num w)
     => Witness w
     -> sig Bool
     -> Ack sig (sig (Enabled a))
     -> Ack sig (sig (Enabled a))
fifo' w rst (Ack outAck inp) = Ack inAck out
   where
           (inAck,out) = fifo w rst (inp,outAck)
           

-- f (Enabled a) (f (Bool) ())



