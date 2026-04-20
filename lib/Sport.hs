-- | UNIX Serial Port
--
-- @
-- import Sport
--
-- main = withSport $ \\s -> do
--   openSport s defSerialConfig
--   bs <- readSport s 64
--   writeSport s bs
-- @
module Sport
  ( -- *** Handle
    Sport
  , withSport
  , newSportIO
  , newSport
  , runSport
  , -- *** Open & Close
    openSport
  , defSportConfig
  , SportConfig(..)
  , BufferMode(..)
  , defSerialConfig
  , SerialConfig(..)
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
