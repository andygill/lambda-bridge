-- | This modules sets up the hslogger details, including catching ^C.

module Network.LambdaBridge.Logging 
        ( init_logging
        , debugM
        ) where

import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals
import System.Environment
import qualified System.Log.Logger as L
import System.Log.Logger hiding (debugM)
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter,close)
import System.Log.Formatter
import System.Posix.Process (getProcessID)

-- use this instead of the debugM.
debugM :: String -> String -> IO ()
debugM logger msg = L.debugM logger msg
-- debugM logger msg = return () 

init_logging :: IO ()
init_logging = do
        prog <- getProgName
        env <- getEnv "LB_LOG" `Prelude.catch` const (return "")
        pid <- getProcessID
        h <- fileHandler (prog ++ "." ++ show pid ++ ".log") DEBUG
        let f = setFormatter h (simpleLogFormatter $ "[" ++ prog ++ " : $loggername : $pid] $msg")
        updateGlobalLogger rootLoggerName (setHandlers [f])

        -- What we want to observe
        updateGlobalLogger "lambda-bridge" (setLevel DEBUG)

        debugM "lambda-bridge" "initialized logging" 

--        installHandler keyboardSignal (CatchOnce $ print "keyboardSignal") Nothing
--        installHandler sigSTOP (CatchOnce $ print "sigSTOP") Nothing
--        installHandler sigINT (CatchOnce $ print "sigINT") Nothing