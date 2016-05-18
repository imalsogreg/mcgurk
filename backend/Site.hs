{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad (forever, join)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.State (gets)
import qualified Data.Aeson as A
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Configurator      as C
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Map.Syntax ((##))
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set as Set
import           Data.Time              (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           GHC.Int
import qualified Heist.Interpreted as I
import           Servant hiding (GET, POST, PUT, DELETE)
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Session
import qualified Snap.Snaplet.Heist as SHeist
import qualified Snap.Snaplet.Heist.Interpreted as I
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import           Web.HttpApiData (parseUrlPiece)
------------------------------------------------------------------------------
import           McGurk.API
import           McGurk.Types
import           Application


apiServer :: Server Api AppHandler
apiServer = atomicallyGetAllRooms :<|> runLecture :<|> undefined :<|> handleRespond

apiApplication :: Application AppHandler
apiApplication = serve apiProxy apiServer

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("api1",   with sess touchSession >> applicationToSnap apiApplication)
         , ("static",      Snap.Util.FileServe.serveDirectory "static")
         , ("/:room", audiencePage)
         ]


atomicallyGetAllRooms :: AppHandler [RoomNumber]
atomicallyGetAllRooms = do
  r <- liftIO . readTVarIO =<< gets _rooms
  return (Map.keys r)


-------------------------------------------------------------------------------
audiencePage :: AppHandler ()
audiencePage = do
  rn <- ( \b -> note "No room param" b >>= parseUrlPiece . T.decodeUtf8 ) <$>
    getParam "room"
  roomsVar <- gets _rooms
  roomLookup <- liftIO $ atomically $ do
    rs <- readTVar roomsVar
    case hush rn >>= flip Map.lookup rs of
      Nothing -> return Nothing
      Just r  -> return $ Just ()
  case roomLookup of
    Nothing -> writeBS "No such room" -- TODO nicer 404 page
    Just r  -> I.render "audience"


runLecture :: AppHandler ()
runLecture = do
  liftIO $ print "run-lecture"
  roomsVar <- gets _rooms
  WS.runWebSocketsSnap $ \pc -> do
    conn <- WS.acceptRequest pc
    n <- atomically $ do
      rooms   <- readTVar roomsVar
      let Just (rNum,_) = Set.minView (Set.difference
                                   (Set.fromList $ map RoomNumber [0..Map.size rooms] )
                                   (Set.fromList $ Map.keys rooms))
      writeTVar roomsVar $ Map.insert rNum (WSRoom conn []) rooms
      return rNum
    wsSendJson conn (ToLecSetRoom $ n)
    forever $ do
      msg :: Maybe T.Text <- wsRecvJson conn
      return ()

handleRespond :: RoomNumber -> Respond -> AppHandler ()
handleRespond room resp = do
  liftIO $ print "RESPOND HANDLER"
  roomsVar <- gets _rooms
  conn <- liftIO $ atomically $ do
    rooms <- readTVar roomsVar
    case Map.lookup room rooms of
      Nothing -> error $ "No such room : " ++ show room
      Just (WSRoom lecConn _) -> return lecConn
  liftIO $ wsSendJson conn (ToLecResponse resp)

wsSendJson :: A.ToJSON a => WS.Connection -> a -> IO ()
wsSendJson conn a = WS.sendTextData conn (A.encode a)

wsRecvJson :: A.FromJSON a => WS.Connection -> IO (Maybe a)
wsRecvJson conn = fmap A.decode $ WS.receiveData conn

-- ------------------------------------------------------------------------------
-- readParam :: (MonadSnap m, Read a) => BS.ByteString -> ExceptT String m a
-- readParam pName = do
--   p <- noteT ("No parameter " <> BS.unpack pName) $ MaybeT $ getParam pName
--   noteT ("Can't read " <> BS.unpack p <> " in param " <> BS.unpack pName) $
--     hoistMaybe (readMay (BS.unpack p))



------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do


    h <- nestSnaplet "" heist $ I.heistInit "templates"
    SHeist.setInterpreted h

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    r <- liftIO $ newTVarIO mempty
    addRoutes routes

    wrapSite (\s -> with sess touchSession >> s >> with sess commitSession)

    return $ App h s r

note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left e) = Nothing
