module Sport.Sport
  ( Sport
  , newSportIO
  , newSport
  , withSport
  , runSport
  , openSport
  , defSportConfig
  , SportConfig(..)
  , closeSport
  , readSport
  , readSomeSport
  , writeSport
  , SportException(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Sport.Serial
import System.IO

newtype Sport = Sport (TVar State)

-- | Acquire IO serial port
newSportIO :: IO Sport
newSportIO = atomically newSport

-- | Acquire STM serial port
newSport :: STM Sport
newSport = Sport <$> newTVar Closed

data State
  = Closed
  | Opening SportConfig (TMVar (Either SomeException ()))
  | Open SportConfig Handle
  | Rd SportConfig Handle Int (TMVar (Either SomeException ByteString))
  | RdSome SportConfig Handle Int (TMVar (Either SomeException ByteString))
  | Wr SportConfig Handle ByteString (TMVar (Either SomeException ()))
  deriving Eq

-- | Handle and serial configuration
data SportConfig = SportConfig
  { binaryMode   :: Bool -- ^ Binary mode True, text mode False.
  , bufferMode   :: BufferMode
  , serialConfig :: SerialConfig
  }
  deriving Eq

-- | Binary with no buffering
defSportConfig :: SportConfig
defSportConfig = SportConfig True NoBuffering defSerialConfig

-- | Open the serial port. Throw 'SportAlreadyOpen' if the
-- serial port was already opened.
openSport :: Sport -> SportConfig -> IO ()
openSport (Sport s) cfg = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed -> writeTVar s $ Opening cfg res
      _      -> throwSTM SportAlreadyOpen
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Close the serial port.
closeSport :: Sport -> IO ()
closeSport (Sport s) = join $ atomically $ do
  st <- readTVar s
  writeTVar s Closed
  case st of
    Closed                 -> return $ return ()
    Opening _          res -> closeResponse res $> return ()
    Open    _ serial       -> return $ hClose serial
    Rd      _ serial _ res -> closeResponse res $> hClose serial
    RdSome  _ serial _ res -> closeResponse res $> hClose serial
    Wr      _ serial _ res -> closeResponse res $> hClose serial

closeResponse :: TMVar (Either SomeException a) -> STM ()
closeResponse res = void $ tryPutTMVar res $ Left $ toException SportClosed

-- | Read n bytes from the serial port. If the serial port is
-- closed then throw 'SportClosed'. Block if the serial port is
-- busy handling a concurrent request or until all n bytes are available.
readSport :: Sport -> Int -> IO ByteString
readSport (Sport s) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ Rd cfg serial n res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Read up to n bytes from the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial port
-- is busy handling a concurrent request or until some of
-- the n bytes are available.
readSomeSport :: Sport -> Int -> IO ByteString
readSomeSport (Sport s) n = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ RdSome cfg serial n res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right bs -> return bs

-- | Write bytes to the serial port. If the serial port
-- is closed then throw 'SportClosed'. Block if the serial
-- port is busy handling a concurrent request.
writeSport :: Sport -> ByteString -> IO ()
writeSport (Sport s) bs = do
  res <- newEmptyTMVarIO
  atomically $ do
    st <- readTVar s
    case st of
      Closed          -> throwSTM SportClosed
      Open cfg serial -> writeTVar s $ Wr cfg serial bs res
      _               -> retry
  atomically $ do
    result <- takeTMVar res
    case result of
      Left err -> throwSTM err
      Right () -> return ()

-- | Acquire a serial port and run the daemon.
withSport :: (Sport -> IO a) -> IO a
withSport k = do
  s <- newSportIO
  either id id <$> race (k s) (runSport s)

-- | Run the serial port daemon and process requests.
runSport :: Sport -> IO a
runSport sp@(Sport s) =
  forever (runState s =<< readTVarIO s) `onException` closeSport sp

runState :: TVar State -> State -> IO ()
runState s st = case st of
  Closed                    -> waitNewState s st
  Opening cfg           res -> handleException res $ opening s cfg res
  Open{}                    -> waitNewState s st
  Rd      cfg serial n  res -> handleException res $ reading s cfg serial n res
  RdSome  cfg serial n  res -> handleException res $ readingSome s cfg serial n res
  Wr      cfg serial bs res -> handleException res $ writing s cfg serial bs res

waitNewState :: TVar State -> State -> IO ()
waitNewState s st = atomically $ check . (st /=) =<< readTVar s

opening :: TVar State -> SportConfig -> TMVar (Either SomeException ()) -> IO ()
opening s cfg res =
  bracketOnError (openSerial $ serialConfig cfg) hClose $ \serial -> do
    hSetBinaryMode serial $ binaryMode cfg
    hSetBuffering serial $ bufferMode cfg
    atomically $ do
      writeTVar s $ Open cfg serial
      writeTMVar res $ Right ()

reading
  :: TVar State
  -> SportConfig
  -> Handle
  -> Int
  -> TMVar (Either SomeException ByteString)
  -> IO ()
reading s cfg serial n res = do
  bs <- BS.hGet serial n
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right bs

readingSome
  :: TVar State
  -> SportConfig
  -> Handle
  -> Int
  -> TMVar (Either SomeException ByteString)
  -> IO ()
readingSome s cfg serial n res = do
  bs <- BS.fromStrict <$> Strict.hGetSome serial n
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right bs

writing
  :: TVar State
  -> SportConfig
  -> Handle
  -> ByteString
  -> TMVar (Either SomeException ())
  -> IO ()
writing s cfg serial bs res = do
  BS.hPut serial bs
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right ()

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
  | SportAlreadyOpen
  deriving (Eq, Read, Show)

instance Exception SportException
