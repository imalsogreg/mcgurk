{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language CPP                 #-}

module Main where

import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Time
import           Data.Aeson
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Document (getLocation)
#endif
import GHCJS.DOM.Location
import           Reflex.Dom

import McGurk.API
import McGurk.Types

main :: IO ()
main = mainWidget run


------------------------------------------------------------------------------
unrelativizeWebSocketUrl :: MonadWidget t m => String -> m String
unrelativizeWebSocketUrl s = do
#ifdef ghcjs_HOST_OS
  doc <- askDocument
  (Just loc) <- getLocation doc
  newProto :: String <- liftIO (getProtocol loc) >>= \case
    ("https:" :: String) -> return "wss:"
    "http:"              -> return "ws:"
  host <- liftIO $ getHost loc
  path <- liftIO $ getPathname loc
  return $ newProto ++ "//" ++ host ++ path ++ s
#else
  error "unrelativieWebSocketUrl is only available from ghcjs"
#endif

------------------------------------------------------------------------------
run :: forall t m.MonadWidget t m => m ()
run = do

  t0 <- liftIO getCurrentTime
  t  <- tickLossy 1 t0
  wsAddr <- unrelativizeWebSocketUrl "api1/run-lecture"
  (WebSocket wsRecv wsOpen) <- webSocket wsAddr $ WebSocketConfig { _webSocketConfig_send = ["pInG"] <$ t }
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

  voteDisplayWidget votes
  el "br" (return ())
  display votes

  where voteIns msg respMap = case msg of
          Just r  -> Map.adjust succ r respMap
          Nothing -> emptyVotes

        emptyVotes = Map.fromList
          (zip (Respond
                <$> [Saw,Heard]
                <*> [minBound..maxBound]) (repeat (0 :: Int)))


------------------------------------------------------------------------------
voteDisplayWidget :: forall t m.MonadWidget t m => Dynamic t (Map Respond Int) -> m ()
voteDisplayWidget votes = do
  let mapCond c = forDyn votes
        (Map.mapKeys (respSyllable) .
         Map.filterWithKey (\k _ -> respModality k == c))
      showMap :: m () -> Dynamic t (Map Syllable Int) -> m ()
      showMap header m = do
        voteSum <- mapDyn (F.foldl' (+) 0 . Map.elems) m
        divClass "vote-results-strip" $
          header >> listWithKey m (showOneVote voteSum) >> return ()
      fI = fromIntegral
      showOneVote :: Dynamic t Int -> Syllable -> Dynamic t Int -> m ()
      showOneVote voteSum k one  = do
        let share n x = if   x == 0
                        then (0 :: Double)
                        else (fI x / fI n :: Double)
            range x0 x1 x = x0 + x * (x1 - x0)
            mkDivAttrs n x = let op = range 0.2 1 $ share n x
                             in  "style" =: ("opacity:" ++ show op)
            mkSpanAttrs n x = let pt = range 30 32 $ share n x
                              in  "style" =: ("font-size:" ++ show (floor pt :: Int) ++ "pt")
        divAttrs  <- combineDyn mkDivAttrs voteSum one
        spanAttrs <- combineDyn mkSpanAttrs voteSum one
        elDynAttr "div" divAttrs $ do
          el "span" $ text (show k)
          elDynAttr "span" spanAttrs $ display one

  sawVotes   <- mapCond Saw
  heardVotes <- mapCond Heard
  showMap (text "testSaw") sawVotes
  showMap (text "testHeard") heardVotes
