-- | UNIX Serial Port
--
-- @
-- import Sport
--
-- main = withSport $ \s -> do
--   openSport s defSerialConfig
--   bs <- readSport s 64
--   writeSport s bs
-- @
module Sport
  ( -- *** Handle
    Sport
  , withSport
  , newSport
  , newSportSTM
  , runSport
  , -- *** Open & Close
    openSport
  , SerialConfig(..)
  , defSerialConfig
  , closeSport
  , -- *** Reading & Writing
    readSport
  , readSomeSport
  , writeSport
  , -- *** Exceptions
    SportException(..)
  ) where

import Sport.Serial
import Sport.Sport
