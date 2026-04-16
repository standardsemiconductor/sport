module Sport.Sport
  ( Sport
  , newSport
  , newSportSTM
  , withSport
  , runSport
  , openSport
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
import Sport.Serial
import System.IO

data Sport = Sport (TVar State)

newSport :: IO Sport
newSport = atomically newSportSTM

newSportSTM :: STM Sport
newSportSTM = Sport <$> newTVar Closed

data State
  = Closed
  | Opening SerialConfig (TMVar (Either SomeException ()))
  | Open SerialConfig Handle
  | Rd SerialConfig Handle Int (TMVar (Either SomeException ByteString))
  | RdSome SerialConfig Handle Int (TMVar (Either SomeException Strict.ByteString))
  | Wr SerialConfig Handle ByteString (TMVar (Either SomeException ()))
  deriving Eq

openSport :: Sport -> SerialConfig -> IO ()
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

readSomeSport :: Sport -> Int -> IO Strict.ByteString
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

withSport :: (Sport -> IO a) -> IO a
withSport k = do
  s <- newSport
  either id id <$> race (k s) (runSport s)

runSport :: Sport -> IO a
runSport sp@(Sport s) = loop `onException` closeSport sp
  where
    loop = forever $ runState s =<< readTVarIO s

closeSport :: Sport -> IO ()
closeSport (Sport s) = join $ atomically $ do
  st <- readTVar s
  writeTVar s Closed
  case st of
    Closed -> return $ return ()
    Opening _ res -> do
      _ <- tryPutTMVar res $ Left $ toException SportClosed
      return $ return ()
    Open _ serial -> return $ hClose serial
    Rd _ serial _ res -> do
      _ <- tryPutTMVar res $ Left $ toException SportClosed
      return $ hClose serial
    RdSome _ serial _ res -> do
      _ <- tryPutTMVar res $ Left $ toException SportClosed
      return $ hClose serial
    Wr _ serial _ res -> do
      _ <- tryPutTMVar res $ Left $ toException SportClosed
      return $ hClose serial

runState :: TVar State -> State -> IO ()
runState s st = case st of
  Closed -> waitNewState s st
  Opening cfg res -> handleException s res $ opening s cfg res
  Open{} -> waitNewState s st
  Rd cfg serial n res -> handleException s res $ reading s cfg serial n res
  RdSome cfg serial n res -> handleException s res $ readingSome s cfg serial n res
  Wr cfg serial bs res -> handleException s res $ writing s cfg serial bs res

waitNewState :: TVar State -> State -> IO ()
waitNewState s st = atomically $ do
  st' <- readTVar s
  check $ st /= st'

opening :: TVar State -> SerialConfig -> TMVar (Either SomeException ()) -> IO ()
opening s cfg res =
  bracketOnError (openSerial cfg) hClose $ \serial ->
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right ()

reading
  :: TVar State
  -> SerialConfig
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
  -> SerialConfig
  -> Handle
  -> Int
  -> TMVar (Either SomeException Strict.ByteString)
  -> IO ()
readingSome s cfg serial n res = do
  bs <- Strict.hGetSome serial n
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right bs

writing
  :: TVar State
  -> SerialConfig
  -> Handle
  -> ByteString
  -> TMVar (Either SomeException ())
  -> IO ()
writing s cfg serial bs res = do
  BS.hPut serial bs
  atomically $ do
    writeTVar s $ Open cfg serial
    writeTMVar res $ Right ()

handleException :: TVar State -> TMVar (Either SomeException a) -> IO () -> IO ()
handleException s res k = k `catches`
  [ Handler $ \e -> do
      closeOnErr s res $ toException e
      throwIO (e :: SomeAsyncException)
  , Handler $ closeOnErr s res
  ]

closeOnErr :: TVar State -> TMVar (Either SomeException a) -> SomeException -> IO ()
closeOnErr s res err = atomically $ do
  _ <- tryPutTMVar res $ Left err
  writeTVar s Closed

data SportException
  = SportClosed
  | SportAlreadyOpen
  deriving (Eq, Read, Show)

instance Exception SportException
