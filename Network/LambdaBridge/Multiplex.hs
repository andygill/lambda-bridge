
module Network.LambdaBridge.Multiplex where
	
import Data.Word
import Data.Binary
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map


import Network.LambdaBridge.Bridge

-- | Takes a super-bridge, and creates many sub-bridges.
-- The assumption is that the outgoing channel of the super-bridge
-- does not block indefinitely, so the sub-bridges will also not block.
-- Further, the super-bridge never block on responses, given
-- the precondition that the sub-bridges also never block on responses.

multiplex :: [Word8] -> Bridge Frame -> IO (Map Word8 (Bridge Frame))
multiplex wds bridge = do

	varMap <- sequence [ do var <- newEmptyMVar 
			        return (i,var)
			   | i <- wds
			   ]

	let varFM = Map.fromList varMap
		
	let loop m = do m ; loop m

	forkIO $ loop $ do
		Frame frame <- fromBridge bridge
		case BS.uncons frame of
		   Just(w,bs) -> 
			case Map.lookup w varFM of
				Nothing -> return () -- bad value, ignored
				Just var -> do
					putMVar var (Frame bs)
					return ()
		   Nothing -> return ()

	return $ Map.fromList
	  [ ( i
	    , Bridge 
			-- Sending blocks, via the super-bridge.
	  	{ toBridge = \ (Frame bs) -> toBridge bridge (Frame (BS.cons i bs))
			-- 
		, fromBridge = takeMVar var
		}
	     )
	  | i <- wds
	  , let (Just var) = Map.lookup i varFM
	  ]

