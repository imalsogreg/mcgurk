{-# language DeriveGeneric     #-}
{-# language OverloadedStrings #-}

module McGurk.Types where

import Data.Aeson
import Data.Monoid ((<>))
import GHC.Generics
import qualified Data.Text as T
import Text.Read (readMaybe)
import Web.HttpApiData

newtype RoomNumber = RoomNumber { room :: Int }
  deriving (Eq,Show, Read, Ord, Generic)

instance ToJSON RoomNumber
instance FromJSON RoomNumber

instance FromHttpApiData RoomNumber where
  parseUrlPiece t = case readMaybe (T.unpack t) of
    Just x -> Right $ RoomNumber x
    Nothing -> Left $ "Unreadable room number: " <> t

data Syllable = Fa | Ga | Ba | Da | Tha
              deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON Syllable
instance FromJSON Syllable

data Modality = Saw | Heard
              deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Modality
instance FromJSON Modality


data Respond = Respond { respModality :: Modality
                       , respSyllable :: Syllable }
             deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Respond
instance FromJSON Respond

data Stimulus = Stimulus { stimVideo :: Syllable
                         , stimAudio :: Syllable
                         } deriving (Eq, Show, Generic)

instance ToJSON Stimulus
instance FromJSON Stimulus

data ToAudienceMsg = ToAudTrialReady
                   | ToAudTrialClosed
                   deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON ToAudienceMsg
instance FromJSON ToAudienceMsg

data ToLecturerMsg = ToLecResponse Respond
                   | ToLecSetRoom  RoomNumber
                   deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON ToLecturerMsg
instance FromJSON ToLecturerMsg
