module Sport.Main (sportMain) where

import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Sport.Sport
import System.IO
import System.Posix

sportMain :: IO ()
sportMain =
  withSport $ \s -> do
    putStrLn "...UNIX.SERIAL.PORT..."
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    openSport s defSportCfg{speed = B19200}
    concurrently_ (reading s) (writing s)

reading :: Sport -> IO a
reading s = forever $ putChar . BS.head =<< readSport s 1

writing :: Sport -> IO a
writing s = forever $ writeSport s . BS.singleton =<< getChar
