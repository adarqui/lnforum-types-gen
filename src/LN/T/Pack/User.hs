{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.User where


import LN.T.Profile
import LN.T.User


import           Control.DeepSeq             (NFData)
import           Data.Aeson                  (FromJSON, ToJSON (), Value (..), parseJSON, toJSON, object, (.=), (.:))
import           Data.Default                (Default, def)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime)
import           Data.Typeable               (Typeable)
import           Data.Monoid                 ((<>))
import           GHC.Generics                (Generic)
import           Haskell.Api.Helpers.Shared  (QueryParam, qp)
import           Prelude

data UserPackResponse = UserPackResponse {
  userPackResponseUser :: !(UserResponse),
  userPackResponseUserId :: !(Int64),
  userPackResponseStat :: !(UserSanitizedStatResponse),
  userPackResponseProfile :: !(ProfileResponse),
  userPackResponseProfileId :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserPackResponse where
  parseJSON (Object o) = do
    userPackResponseUser <- o .: ("user")
    userPackResponseUserId <- o .: ("user_id")
    userPackResponseStat <- o .: ("stat")
    userPackResponseProfile <- o .: ("profile")
    userPackResponseProfileId <- o .: ("profile_id")
    pure $ UserPackResponse {
      userPackResponseUser = userPackResponseUser,
      userPackResponseUserId = userPackResponseUserId,
      userPackResponseStat = userPackResponseStat,
      userPackResponseProfile = userPackResponseProfile,
      userPackResponseProfileId = userPackResponseProfileId
    }
  parseJSON x = fail $ "UserPackResponse: Could not parse object: " <> show x


instance ToJSON UserPackResponse where
  toJSON UserPackResponse{..} = object $
    [ "tag" .= ("UserPackResponse" :: Text)
    , "user" .= userPackResponseUser
    , "user_id" .= userPackResponseUserId
    , "stat" .= userPackResponseStat
    , "profile" .= userPackResponseProfile
    , "profile_id" .= userPackResponseProfileId
    ]


instance Eq UserPackResponse where
  (==) a b = userPackResponseUser a == userPackResponseUser b && userPackResponseUserId a == userPackResponseUserId b && userPackResponseStat a == userPackResponseStat b && userPackResponseProfile a == userPackResponseProfile b && userPackResponseProfileId a == userPackResponseProfileId b

instance Show UserPackResponse where
    show rec = "userPackResponseUser: " <> show (userPackResponseUser rec) <> ", " <> "userPackResponseUserId: " <> show (userPackResponseUserId rec) <> ", " <> "userPackResponseStat: " <> show (userPackResponseStat rec) <> ", " <> "userPackResponseProfile: " <> show (userPackResponseProfile rec) <> ", " <> "userPackResponseProfileId: " <> show (userPackResponseProfileId rec)

data UserPackResponses = UserPackResponses {
  userPackResponses :: !([UserPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON UserPackResponses where
  parseJSON (Object o) = do
    userPackResponses <- o .: ("user_pack_responses")
    pure $ UserPackResponses {
      userPackResponses = userPackResponses
    }
  parseJSON x = fail $ "UserPackResponses: Could not parse object: " <> show x


instance ToJSON UserPackResponses where
  toJSON UserPackResponses{..} = object $
    [ "tag" .= ("UserPackResponses" :: Text)
    , "user_pack_responses" .= userPackResponses
    ]


instance Eq UserPackResponses where
  (==) a b = userPackResponses a == userPackResponses b

instance Show UserPackResponses where
    show rec = "userPackResponses: " <> show (userPackResponses rec)
-- footer
