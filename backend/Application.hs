{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Data.Map (Map)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import qualified Network.WebSockets as WS
import McGurk.Types

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _rooms :: TVar (Map RoomNumber WSRoom)
    }

data WSRoom = WSRoom {
    _lectureConnecion :: WS.Connection
  , _audienceConnections :: [WS.Connection]
  }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
type AppHandler = Handler App App
