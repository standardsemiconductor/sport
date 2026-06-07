-- | UNIX Serial Port
--
-- @
-- import Sport
--
-- main = do
--   s <- newSportIO
--   openSport s defSportCfg{path="\/dev\/ttyUSB1", speed=B19200}
--   bs <- readSport s 64
--   writeSport s bs
-- @
module Sport
  ( -- *** Handle
    Sport
  , withNewSport
  , newSportIO
  , newSport
  , -- *** Open & Close
    withSport
  , openSport
  , isOpenSport
  , getSportCfg
  , defSportCfg
  , SportCfg(..)
  , BufferMode(..)
  , BaudRate(..)
  , Parity(..)
  , StopBits(..)
  , closeSport
  , -- *** Read & Write
    readSport
  , readSomeSport
  , writeSport
  , flushSport
  , -- *** Exception
    SportException(..)
  , displaySportException
  ) where

import Sport.Serial
import Sport.Sport
import System.IO
import System.Posix
