-- | UNIX Serial Port
--
-- @
-- import Sport
--
-- main = withSport $ \\s -> do
--   openSport s defSportCfg{path="\/dev\/ttyUSB1", speed=B19200}
--   bs <- readSport s 64
--   writeSport s bs
-- @
module Sport
  ( -- *** Handle & Daemon
    Sport
  , withSport
  , newSportIO
  , newSport
  , runSport
  , -- *** Open & Close
    openSport
  , defSportCfg
  , SportCfg(..)
  , BufferMode(..)
  , Parity(..)
  , StopBits(..)
  , closeSport
  , -- *** Read & Write
    readSport
  , readSomeSport
  , writeSport
  , -- *** Exception
    SportException(..)
  ) where

import Sport.Serial
import Sport.Sport
import System.IO
