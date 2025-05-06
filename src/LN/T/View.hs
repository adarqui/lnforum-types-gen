{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.View where


import LN.T.Ent


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

data ViewRequest = ViewRequest {
  viewRequestCount :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ViewRequest where
  parseJSON (Object o) = do
    viewRequestCount <- o .: ("count" :: Data.Aeson.Key.Key)
    pure $ ViewRequest {
      viewRequestCount = viewRequestCount
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ViewRequest where
  toJSON ViewRequest{..} = object $
    [ "tag" .= ("ViewRequest" :: Text)
    , "count" .= viewRequestCount
    ]


instance Eq ViewRequest where
  (==) a b = viewRequestCount a == viewRequestCount b

instance Show ViewRequest where
    show rec = "viewRequestCount: " <> show (viewRequestCount rec)

data ViewResponse = ViewResponse {
  viewResponseId :: !(Int64),
  viewResponseEnt :: !(Ent),
  viewResponseEntId :: !(Int64),
  viewResponseCount :: !(Int64),
  viewResponseCreatedAt :: !((Maybe UTCTime)),
  viewResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ViewResponse where
  parseJSON (Object o) = do
    viewResponseId <- o .: ("id" :: Data.Aeson.Key.Key)
    viewResponseEnt <- o .: ("ent" :: Data.Aeson.Key.Key)
    viewResponseEntId <- o .: ("ent_id" :: Data.Aeson.Key.Key)
    viewResponseCount <- o .: ("count" :: Data.Aeson.Key.Key)
    viewResponseCreatedAt <- o .: ("created_at" :: Data.Aeson.Key.Key)
    viewResponseModifiedAt <- o .: ("modified_at" :: Data.Aeson.Key.Key)
    pure $ ViewResponse {
      viewResponseId = viewResponseId,
      viewResponseEnt = viewResponseEnt,
      viewResponseEntId = viewResponseEntId,
      viewResponseCount = viewResponseCount,
      viewResponseCreatedAt = viewResponseCreatedAt,
      viewResponseModifiedAt = viewResponseModifiedAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ViewResponse where
  toJSON ViewResponse{..} = object $
    [ "tag" .= ("ViewResponse" :: Text)
    , "id" .= viewResponseId
    , "ent" .= viewResponseEnt
    , "ent_id" .= viewResponseEntId
    , "count" .= viewResponseCount
    , "created_at" .= viewResponseCreatedAt
    , "modified_at" .= viewResponseModifiedAt
    ]


instance Eq ViewResponse where
  (==) a b = viewResponseId a == viewResponseId b && viewResponseEnt a == viewResponseEnt b && viewResponseEntId a == viewResponseEntId b && viewResponseCount a == viewResponseCount b && viewResponseCreatedAt a == viewResponseCreatedAt b && viewResponseModifiedAt a == viewResponseModifiedAt b

instance Show ViewResponse where
    show rec = "viewResponseId: " <> show (viewResponseId rec) <> ", " <> "viewResponseEnt: " <> show (viewResponseEnt rec) <> ", " <> "viewResponseEntId: " <> show (viewResponseEntId rec) <> ", " <> "viewResponseCount: " <> show (viewResponseCount rec) <> ", " <> "viewResponseCreatedAt: " <> show (viewResponseCreatedAt rec) <> ", " <> "viewResponseModifiedAt: " <> show (viewResponseModifiedAt rec)

data ViewResponses = ViewResponses {
  viewResponses :: !([ViewResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ViewResponses where
  parseJSON (Object o) = do
    viewResponses <- o .: ("view_responses" :: Data.Aeson.Key.Key)
    pure $ ViewResponses {
      viewResponses = viewResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ViewResponses where
  toJSON ViewResponses{..} = object $
    [ "tag" .= ("ViewResponses" :: Text)
    , "view_responses" .= viewResponses
    ]


instance Eq ViewResponses where
  (==) a b = viewResponses a == viewResponses b

instance Show ViewResponses where
    show rec = "viewResponses: " <> show (viewResponses rec)
-- footer
