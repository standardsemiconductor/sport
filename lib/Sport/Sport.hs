-- | Internal high-level serial port
module Sport.Sport
  ( Sport
  , newSportIO
  , newSport
  , withSport
  , withOpenSport
  , runSport
  , openSport
  , isOpenSport
  , getSportCfg
  , getOpenSportCfg
  , defSportCfg
  , SportCfg(..)
  , closeSport
  , readSport
  , readSomeSport
  , writeSport
  , flushSport
  , SportException(..)
  , displaySportException
  ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Data.Maybe
import qualified Sport.Serial as S
import System.IO
import System.Posix

-- | Serial port handle
newtype Sport = Sport (TVar State)

-- | Acquire IO serial port
newSportIO :: IO Sport
newSportIO = atomically newSport

-- | Acquire STM serial port
newSport :: STM Sport
newSport = Sport <$> newTVar Closed

data State
  = Closed
  | Opening SportCfg (TMVar (Either SomeException ()))
  | Open SportCfg Handle
  deriving Eq

readHandle :: Sport -> STM Handle
readHandle (Sport s) = do
  st <- readTVar s
  case st of
    Closed    -> retry
    Opening{} -> retry
    Open _ h  -> return h

tryReadHandle :: Sport -> STM (Maybe Handle)
tryReadHandle s = Just `fmap` readHandle s <|> pure Nothing

-- | Check if serial port is open
isOpenSport :: Sport -> STM Bool
isOpenSport s = isJust <$> tryReadHandle s

-- | Handle and serial configuration
data SportCfg = SportCfg
  { binaryMode :: Bool -- ^ Binary mode True, text mode False.
  , bufferMode :: BufferMode
  , path       :: FilePath
  , speed      :: BaudRate
  , byteSize   :: Int -- ^ number of bits per byte
  , parity     :: Maybe S.Parity
  , stopBits   :: S.StopBits
  , rtimeout   :: Maybe Int
  , excl       :: Bool -- ^ exclusive
  }
  deriving (Eq, Show)

-- | Binary with no buffering
defSportCfg :: SportCfg
defSportCfg =
  SportCfg
    True
    NoBuffering
    (S.path S.defSerialCfg)
    (S.speed S.defSerialCfg)
    (S.byteSize S.defSerialCfg)
    (S.parity S.defSerialCfg)
    (S.stopBits S.defSerialCfg)
    (S.rtimeout S.defSerialCfg)
    (S.excl S.defSerialCfg)

-- | Open the serial port. Throw 'SportAlreadyOpen' if the
-- serial port was already opened.
openSport :: Sport -> SportCfg -> IO ()
openSport s@(Sport state) cfg' = do
  res <- newEmptyTMVarIO
  atomically $ do
    cfgM <- getSportCfg s
    case cfgM of
      Nothing  -> writeTVar state $ Opening cfg' res
      Just cfg -> throwSTM $ SportAlreadyOpen $ path cfg
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Get current configuration. Returns 'Just' if
-- the current state is opening or open. See also 'getOpenSportCfg'.
getSportCfg :: Sport -> STM (Maybe SportCfg)
getSportCfg (Sport s) = do
  st <- readTVar s
  return $ case st of
    Closed        -> Nothing
    Opening cfg _ -> Just cfg
    Open    cfg _ -> Just cfg

-- | Get current open configuration. Returns 'Just' if
-- the current state is open. See also 'getSportCfg'.
getOpenSportCfg :: Sport -> STM (Maybe SportCfg)
getOpenSportCfg (Sport s) = do
  st <- readTVar s
  return $ case st of
    Closed     -> Nothing
    Opening{}  -> Nothing
    Open cfg _ -> Just cfg

-- | Close the serial port.
closeSport :: Sport -> IO ()
closeSport (Sport s) = join $ atomically $ do
  st <- readTVar s
  writeTVar s Closed
  closeState st

closeState :: State -> STM (IO ())
closeState Closed          = return $ return ()
closeState (Opening _ res) = closeResponse res $> return ()
closeState (Open _ serial) = return $ hClose serial

closeResponse :: TMVar (Either SomeException a) -> STM ()
closeResponse res = void $ tryPutTMVar res $ Left $ toException SportClosed

-- | Read n bytes from the serial port. If the serial port is
-- closed then throw 'SportClosed'. Blocks until all n bytes
-- are available.
readSport :: Sport -> Int -> IO ByteString
readSport s n = do
  h <- atomically $ readHandle s <|> throwSTM SportClosed
  BS.hGet h n

-- | Read up to n bytes from the serial port. If the serial port
-- is closed then throw 'SportClosed'. Blocks until some of
-- the n bytes are available.
readSomeSport :: Sport -> Int -> IO ByteString
readSomeSport s n = do
  h <- atomically $ readHandle s <|> throwSTM SportClosed
  BS.fromStrict <$> Strict.hGetSome h n

-- | Write bytes to the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial
-- port is busy handling a concurrent request.
writeSport :: Sport -> ByteString -> IO ()
writeSport s bs = do
  h <- atomically $ readHandle s <|> throwSTM SportClosed
  BS.hPut h bs

-- | Flush the serial handle. Do nothing if closed.
flushSport :: Sport -> IO ()
flushSport s = do
  hM <- atomically $ tryReadHandle s
  mapM_ hFlush hM

-- | Acquire a serial port and run the daemon.
withSport :: (Sport -> IO a) -> IO a
withSport k = do
  s <- newSportIO
  either id id <$> race (k s) (runSport s)

-- | Construct the handle, run the daemon,
-- and open the configured serial port. The serial port
-- is closed on leaving scope.
withOpenSport :: SportCfg -> (Sport -> IO a) -> IO a
withOpenSport cfg k =
  withSport $ \s -> bracket_ (openSport s cfg) (closeSport s) (k s)

-- | Run the serial port daemon and process requests.
runSport :: Sport -> IO a
runSport s@(Sport state) =
  forever (runState s =<< readTVarIO state) `onException` closeSport s

runState :: Sport -> State -> IO ()
runState s@(Sport state) st = case st of
  Closed -> waitNewState s st
  Opening cfg res ->
    handleException res $
      opening s cfg res
        `onException` atomically (writeTVar state Closed)
  Open{} -> waitNewState s st

waitNewState :: Sport -> State -> IO ()
waitNewState (Sport s) st = atomically $ check . (st /=) =<< readTVar s

opening :: Sport -> SportCfg -> TMVar (Either SomeException ()) -> IO ()
opening (Sport s) cfg res =
  bracketOnError (S.openSerial $ toSerialCfg cfg) hClose $ \serial -> do
    hSetBinaryMode serial $ binaryMode cfg
    hSetBuffering serial $ bufferMode cfg
    atomically $ do
      writeTVar s $ Open cfg serial
      writeTMVar res $ Right ()

toSerialCfg :: SportCfg -> S.SerialCfg
toSerialCfg s =
  S.SerialCfg
    (path s)
    (speed s)
    (byteSize s)
    (parity s)
    (stopBits s)
    (rtimeout s)
    (excl s)

handleException :: TMVar (Either SomeException a) -> IO () -> IO ()
handleException res k = k `catches`
  [ Handler $ \e -> throwIO (e :: SomeAsyncException)
  , Handler $ \e -> atomically $ void $ tryPutTMVar res $ Left (e :: SomeException)
  ]

-- | Serial port exceptions
data SportException
    -- | User required an open sport but it was closed.
  = SportClosed
    -- | User attempted to open a serial port that was already open.
  | SportAlreadyOpen String
  deriving (Eq, Read, Show)

instance Exception SportException

-- | Pretty exception rendering
displaySportException :: SportException -> String
displaySportException e = "sport exception: " <> case e of
  SportClosed -> "serial port closed"
  SportAlreadyOpen p -> "serial port already open: " <> p
