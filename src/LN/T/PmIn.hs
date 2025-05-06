{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.PmIn where





import           Control.DeepSeq             (NFData)
import           Data.Aeson.Key
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

data PmInRequest = PmInRequest {
  pmInRequestLabel :: !((Maybe Text)),
  pmInRequestIsRead :: !(Bool),
  pmInRequestIsStarred :: !(Bool),
  pmInRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmInRequest where
  parseJSON (Object o) = do
    pmInRequestLabel <- o .: ("label" :: Data.Aeson.Key.Key)
    pmInRequestIsRead <- o .: ("is_read" :: Data.Aeson.Key.Key)
    pmInRequestIsStarred <- o .: ("is_starred" :: Data.Aeson.Key.Key)
    pmInRequestGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    pure $ PmInRequest {
      pmInRequestLabel = pmInRequestLabel,
      pmInRequestIsRead = pmInRequestIsRead,
      pmInRequestIsStarred = pmInRequestIsStarred,
      pmInRequestGuard = pmInRequestGuard
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmInRequest where
  toJSON PmInRequest{..} = object $
    [ "tag" .= ("PmInRequest" :: Text)
    , "label" .= pmInRequestLabel
    , "is_read" .= pmInRequestIsRead
    , "is_starred" .= pmInRequestIsStarred
    , "guard" .= pmInRequestGuard
    ]


instance Eq PmInRequest where
  (==) a b = pmInRequestLabel a == pmInRequestLabel b && pmInRequestIsRead a == pmInRequestIsRead b && pmInRequestIsStarred a == pmInRequestIsStarred b && pmInRequestGuard a == pmInRequestGuard b

instance Show PmInRequest where
    show rec = "pmInRequestLabel: " <> show (pmInRequestLabel rec) <> ", " <> "pmInRequestIsRead: " <> show (pmInRequestIsRead rec) <> ", " <> "pmInRequestIsStarred: " <> show (pmInRequestIsStarred rec) <> ", " <> "pmInRequestGuard: " <> show (pmInRequestGuard rec)

data PmInResponse = PmInResponse {
  pmInResponseId :: !(Int64),
  pmInResponsePmId :: !(Int64),
  pmInResponseUserId :: !(Int64),
  pmInResponseLabel :: !((Maybe Text)),
  pmInResponseIsRead :: !(Bool),
  pmInResponseIsStarred :: !(Bool),
  pmInResponseIsNew :: !(Bool),
  pmInResponseIsSaved :: !(Bool),
  pmInResponseActive :: !(Bool),
  pmInResponseGuard :: !(Int),
  pmInResponseCreatedAt :: !((Maybe UTCTime)),
  pmInResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmInResponse where
  parseJSON (Object o) = do
    pmInResponseId <- o .: ("id" :: Data.Aeson.Key.Key)
    pmInResponsePmId <- o .: ("pm_id" :: Data.Aeson.Key.Key)
    pmInResponseUserId <- o .: ("user_id" :: Data.Aeson.Key.Key)
    pmInResponseLabel <- o .: ("label" :: Data.Aeson.Key.Key)
    pmInResponseIsRead <- o .: ("is_read" :: Data.Aeson.Key.Key)
    pmInResponseIsStarred <- o .: ("is_starred" :: Data.Aeson.Key.Key)
    pmInResponseIsNew <- o .: ("is_new" :: Data.Aeson.Key.Key)
    pmInResponseIsSaved <- o .: ("is_saved" :: Data.Aeson.Key.Key)
    pmInResponseActive <- o .: ("active" :: Data.Aeson.Key.Key)
    pmInResponseGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    pmInResponseCreatedAt <- o .: ("created_at" :: Data.Aeson.Key.Key)
    pmInResponseModifiedAt <- o .: ("modified_at" :: Data.Aeson.Key.Key)
    pure $ PmInResponse {
      pmInResponseId = pmInResponseId,
      pmInResponsePmId = pmInResponsePmId,
      pmInResponseUserId = pmInResponseUserId,
      pmInResponseLabel = pmInResponseLabel,
      pmInResponseIsRead = pmInResponseIsRead,
      pmInResponseIsStarred = pmInResponseIsStarred,
      pmInResponseIsNew = pmInResponseIsNew,
      pmInResponseIsSaved = pmInResponseIsSaved,
      pmInResponseActive = pmInResponseActive,
      pmInResponseGuard = pmInResponseGuard,
      pmInResponseCreatedAt = pmInResponseCreatedAt,
      pmInResponseModifiedAt = pmInResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmInResponse where
  toJSON PmInResponse{..} = object $
    [ "tag" .= ("PmInResponse" :: Text)
    , "id" .= pmInResponseId
    , "pm_id" .= pmInResponsePmId
    , "user_id" .= pmInResponseUserId
    , "label" .= pmInResponseLabel
    , "is_read" .= pmInResponseIsRead
    , "is_starred" .= pmInResponseIsStarred
    , "is_new" .= pmInResponseIsNew
    , "is_saved" .= pmInResponseIsSaved
    , "active" .= pmInResponseActive
    , "guard" .= pmInResponseGuard
    , "created_at" .= pmInResponseCreatedAt
    , "modified_at" .= pmInResponseModifiedAt
    ]


instance Eq PmInResponse where
  (==) a b = pmInResponseId a == pmInResponseId b && pmInResponsePmId a == pmInResponsePmId b && pmInResponseUserId a == pmInResponseUserId b && pmInResponseLabel a == pmInResponseLabel b && pmInResponseIsRead a == pmInResponseIsRead b && pmInResponseIsStarred a == pmInResponseIsStarred b && pmInResponseIsNew a == pmInResponseIsNew b && pmInResponseIsSaved a == pmInResponseIsSaved b && pmInResponseActive a == pmInResponseActive b && pmInResponseGuard a == pmInResponseGuard b && pmInResponseCreatedAt a == pmInResponseCreatedAt b && pmInResponseModifiedAt a == pmInResponseModifiedAt b

instance Show PmInResponse where
    show rec = "pmInResponseId: " <> show (pmInResponseId rec) <> ", " <> "pmInResponsePmId: " <> show (pmInResponsePmId rec) <> ", " <> "pmInResponseUserId: " <> show (pmInResponseUserId rec) <> ", " <> "pmInResponseLabel: " <> show (pmInResponseLabel rec) <> ", " <> "pmInResponseIsRead: " <> show (pmInResponseIsRead rec) <> ", " <> "pmInResponseIsStarred: " <> show (pmInResponseIsStarred rec) <> ", " <> "pmInResponseIsNew: " <> show (pmInResponseIsNew rec) <> ", " <> "pmInResponseIsSaved: " <> show (pmInResponseIsSaved rec) <> ", " <> "pmInResponseActive: " <> show (pmInResponseActive rec) <> ", " <> "pmInResponseGuard: " <> show (pmInResponseGuard rec) <> ", " <> "pmInResponseCreatedAt: " <> show (pmInResponseCreatedAt rec) <> ", " <> "pmInResponseModifiedAt: " <> show (pmInResponseModifiedAt rec)

data PmInResponses = PmInResponses {
  pmInResponses :: !([PmInResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON PmInResponses where
  parseJSON (Object o) = do
    pmInResponses <- o .: ("pm_in_responses" :: Data.Aeson.Key.Key)
    pure $ PmInResponses {
      pmInResponses = pmInResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON PmInResponses where
  toJSON PmInResponses{..} = object $
    [ "tag" .= ("PmInResponses" :: Text)
    , "pm_in_responses" .= pmInResponses
    ]


instance Eq PmInResponses where
  (==) a b = pmInResponses a == pmInResponses b

instance Show PmInResponses where
    show rec = "pmInResponses: " <> show (pmInResponses rec)
-- footer
