{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}

module Main where

import Reflex.Dom
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time
import Data.Aeson
import GHCJS.DOM.Document (getLocation)
import GHCJS.DOM.Location

import McGurk.API
import McGurk.Types

main :: IO ()
main = mainWidget run

unrelativizeWebSocketUrl :: MonadWidget t m => String -> m String
unrelativizeWebSocketUrl s = do
  doc <- askDocument
  (Just loc) <- getLocation doc
  newProto :: String <- liftIO (getProtocol loc) >>= \case
    ("https:" :: String) -> return "wss:"
    "http:"              -> return "ws:"
  host <- liftIO $ getHost loc
  path <- liftIO $ getPathname loc
  return $ newProto ++ "//" ++ host ++ path ++ s

------------------------------------------------------------------------------
run :: forall t m.MonadWidget t m => m ()
run = do

  t0 <- liftIO getCurrentTime
  t  <- tickLossy 1 t0
  wsAddr <- unrelativizeWebSocketUrl "api1/run-lecture"
  let wsAddr2 = "ws://localhost:5000/api1/run-lecture"
  (WebSocket wsRecv wsOpen) <- webSocket wsAddr2 $ WebSocketConfig { _webSocketConfig_send = ["pInG"] <$ t }
  let msgs :: Event t (Maybe ToLecturerMsg) = fmap (decode . BSL.fromStrict) wsRecv

  dynText =<< holdDyn "No message" (leftmost ["WS Open" <$ wsOpen, fmap BS.unpack wsRecv])

  roomNum <- holdDyn Nothing $ ffor msgs $ \case
    Just (ToLecSetRoom r) -> Just r
    _                     -> Nothing

  voteClear <- button "Clear votes"

  let voteMsgs  = fforMaybe msgs $ \case
        Just (ToLecResponse r) -> Just r
        _                      -> Nothing
      voteBumps = leftmost [fmap Just voteMsgs, Nothing <$ voteClear]
  votes <- foldDyn voteIns emptyVotes voteBumps

  display votes

  where voteIns msg respMap = case msg of
          Just r  -> Map.adjust succ r respMap
          Nothing -> emptyVotes

        emptyVotes = Map.fromList (zip (Respond <$> [Saw,Heard] <*> [minBound..maxBound]) (repeat (0 :: Int)))
