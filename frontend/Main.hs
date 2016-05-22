{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language CPP                 #-}
{-# language RecursiveDo         #-}

module Main where

import           Control.Monad (join, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid ((<>))
import           Data.Time
import           Data.Aeson
import           GHCJS.DOM.EventM (on)
import           GHCJS.DOM.Types (HTMLDocument)
import           GHCJS.DOM.Document (keyPress)
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Document (getLocation)
#endif
import GHCJS.DOM.Location
import           Reflex.Dom

import McGurk.API
import McGurk.Types

------------------------------------------------------------------------------
main :: IO ()
main = mainWidget run


-------------------------------------------------------------------------------
data ExperimentState = ExperimentState
  { esAudio :: Maybe Syllable
  , esVideo :: Maybe Syllable
  , esReveal   :: Bool
  }


-------------------------------------------------------------------------------
data StateUpdate = Reveal
                 | SetAudio Syllable
                 | SetVideo Syllable
                 | Clear


-------------------------------------------------------------------------------
keyActions :: Map Char StateUpdate
keyActions = Map.fromList $
  ('\n', Reveal) :
  zipWith (\c s -> (c, SetAudio s)) "fgbdt" [Fa,Ga,Ba,Da,Tha] ++
  zipWith (\c s -> (c, SetVideo s)) "FGBDT" [Fa,Ga,Ba,Da,Tha]


-------------------------------------------------------------------------------
videoUrls :: Map Syllable String
videoUrls =
  map (\s -> (s, "https://cbmm-mcgurk.s3.amazonaws/" ++ show s ++ ".mp4"))
  [minBound..maxBound]


-------------------------------------------------------------------------------
audioUrls :: Map Syllable String
audioUrls =
  map (\s -> (s, "https://cbmm-mcgurk.s3.amazonaws/" ++ show s ++ ".mp3"))
  [minBound..maxBound]


-------------------------------------------------------------------------------
stateTransition :: StateUpdate -> ExperimentState -> ExperimentState
stateTransition Clear _ = ExperimentState Nothing Nothing False
stateTransition (SetAudio s) es = es { esAudio = Just $ fromMaybe s (esAudio es)}
stateTransition (SetVideo s) es = es { esVideo = Just $ fromMaybe s (esVideo es)}
stateTransition Reveal       es = es { esReveal = True }


-------------------------------------------------------------------------------
-- Stimuli and instructions for choosing stimuli
stimulusWidget :: Dynamic t ExperimentState -> m ()
stimulusWidget experState = divClass "video-widget" $ do
  vid <- forDyn experState $ \es -> videoWidget "video" []


-------------------------------------------------------------------------------
-- Main page logic & elements
run :: forall t m.MonadWidget t m => m ()
run = do

  --------------------------------------
  -- Setup
  doc  <- askDocument
  keys :: Event t Char <- fmap toEnum <$> wrapDomEvent doc
    (`on` keyPress) getKeyEvent
  let keyUpdates = fmapMaybe (flip Map.lookup keyActions) keys

  t0   <- liftIO getCurrentTime
  tick <- tickLossy 1 t0

  rec wsAddr <- unrelativizeWebSocketUrl "api1/run-lecture"
      (WebSocket wsRecv wsOpen) <- webSocket wsAddr $
        WebSocketConfig { _webSocketConfig_send =
                          leftmost [["open"] <$ openVotes
                                   ,["pInG"] <$ tick
                                   ]}

      let msgs :: Event t (Maybe ToLecturerMsg) =
            fmap (decode . BSL.fromStrict) wsRecv

      roomNum <- holdDyn Nothing $ fmap Just $ fforMaybe msgs $ \case
        Just (ToLecSetRoom r) -> Just r
        _                     -> Nothing

      contentsAttrs <- forDyn roomNum $ \n ->
        "class" =: "contents" <> "style" =: ("background-color: " ++ roomColor n)

      --------------------------------------
      -- Visible elements
      expState <- elDynAttr "div" contentsAttrs $ do
        reveal <- fmap (const Reveal) <$> button "Reveal"

        expState <- foldDyn stateTransition (ExperimentState Nothing Nothing False)
          (leftmost [keyUpdates, reveal])
        let voteMsgs  = fforMaybe msgs $ \case
              Just (ToLecResponse r) -> Just r
              _                      -> Nothing
            voteClear = ffilter esReveal $ updated expState
            voteBumps = leftmost [fmap Just voteMsgs, Nothing <$ voteClear]
        votes <- foldDyn voteIns emptyVotes voteBumps

        voteDisplayWidget roomNum expState votes
        return expState
      let openVotes = ffilter (\es -> isJust (esAudio es) && isJust (esVideo es)) (updated expState)

  return ()

  where voteIns msg respMap = case msg of
          Just r  -> Map.adjust succ r respMap
          Nothing -> emptyVotes

        emptyVotes = Map.fromList
          (zip (Respond
                <$> [Saw,Heard]
                <*> [minBound..maxBound]) (repeat (0 :: Int)))


-------------------------------------------------------------------------------
-- Display a link to the audience's response panel for this room
roomLinkWidget :: MonadWidget t m => Dynamic t (Maybe RoomNumber) -> m ()
roomLinkWidget rn = divClass "room-link" $ do
  dyn =<< (forDyn rn $ \case
              Nothing -> text ""
              Just n  -> do
                href <- audienceRoomUrl n
                elAttr "a" ("href" =: href) $ text href)
  return ()

------------------------------------------------------------------------------
-- Show the accumulated votes for each modality and syllable
voteDisplayWidget :: forall t m.MonadWidget t m
                  => Dynamic t (Maybe RoomNumber)
                  -> Dynamic t ExperimentState
                  -> Dynamic t (Map Respond Int)
                  -> m ()
voteDisplayWidget roomNum expState votes = do

  let mapCond c = forDyn votes
        (Map.mapKeys (respSyllable) .
         Map.filterWithKey (\k _ -> respModality k == c))

      showMap :: m () -> Modality -> Dynamic t (Map Syllable Int) -> m ()
      showMap header modality m = do
        voteMax <- mapDyn (F.foldl' (max) 0 . Map.elems) m
        divClass "vote-results-strip" $
          header >> listWithKey m (showOneVote voteMax modality) >> return ()
      fI = fromIntegral

      showOneVote :: Dynamic t Int -> Modality -> Syllable -> Dynamic t Int -> m ()
      showOneVote voteMax modality k one  = do
        let share n x = if   x == 0
                        then (0 :: Double)
                        else (fI x / fI n :: Double)
            range x0 x1 x = x0 + x * (x1 - x0)
            mkDivAttrs rn n x es =
              let op = range 0.5 1 $ share n x
              in  "style" =: ("opacity:" ++ show op ++
                              ";color:" ++ roomColor rn)
                  <> bool mempty ("class" =: "selected") (isSelected es modality k)
            mkSpanAttrs n x =
              let pt = range 30 30 $ share n x
              in  "style" =: ("font-size:" ++ show (floor pt :: Int) ++ "pt")

        divAttrs  <-  mkDivAttrs `mapDyn` roomNum `apDyn` voteMax
                                 `apDyn`  one     `apDyn` expState
        spanAttrs <- combineDyn mkSpanAttrs voteMax one
        elDynAttr "div" divAttrs $ do
          el "span" $ text (show k)
          elDynAttr "span" spanAttrs $ display one

      bootstrapIcon n = do
        spanAttrs <- forDyn roomNum $ \rn -> "style" =: ("color:" ++ roomColor rn)
                                             <> "class" =: ("glyphicon glyphicon-" ++ n)
                                             <> "aria-hidden" =: "true"
        elDynAttr "span" spanAttrs (return ())

  sawVotes   <- mapCond Saw
  heardVotes <- mapCond Heard
  showMap (bootstrapIcon "headphones") Heard heardVotes
  showMap (bootstrapIcon "eye-open") Saw sawVotes


------------------------------------------------------------------------------
isSelected :: ExperimentState -> Modality -> Syllable -> Bool
isSelected es modality syllable = esReveal es && case modality of
  Saw   -> esVideo es == Just syllable
  Heard -> esAudio es == Just syllable


------------------------------------------------------------------------------
apDyn :: (MonadHold t m, Reflex t)
      => m (Dynamic t (a -> b))
      -> Dynamic t a
      -> m (Dynamic t b)
apDyn mf a = do
  f <- mf
  combineDyn ($) f a



#ifdef ghcjs_HOST_OS
#else
getLocation :: HTMLDocument -> IO (Maybe Location)
getLocation = error "getLocation only available to ghcjs"
getProtocol :: Location -> IO String
getProtocol = error "getProtocol only available to ghcjs"
getHost :: Location -> IO String
getHost = error "getHost only available to ghcjs"
getPathname :: Location -> IO String
getPathname = error "getPathname only available to ghcjs"
#endif

------------------------------------------------------------------------------
unrelativizeWebSocketUrl :: MonadWidget t m => String -> m String
unrelativizeWebSocketUrl s = do
  doc <- askDocument
  (Just loc) <- liftIO $ getLocation doc
  newProto :: String <- liftIO (getProtocol loc) >>= \case
    ("https:" :: String) -> return "wss:"
    "http:"              -> return "ws:"
  host <- liftIO $ getHost loc
  path <- liftIO $ getPathname loc
  return $ newProto ++ "//" ++ host ++ path ++ s

audienceRoomUrl :: MonadWidget t m => RoomNumber -> m String
audienceRoomUrl (RoomNumber n) = do
  doc <- askDocument
  (Just loc) <- liftIO $ getLocation doc
  liftIO $ liftM concat $ sequence [getProtocol loc
                                   , return "//"
                                   , getHost loc
                                   , pure (show n)]


------------------------------------------------------------------------------
roomColor :: Maybe RoomNumber -> String
roomColor Nothing = "hsla(0,0%,57%,1)"
roomColor (Just (RoomNumber n)) =
  let h = show $ (floor $ fromIntegral n * 300/7 :: Int)
  in "hsla(" ++ h ++ ", 29%, 57%,1)"

