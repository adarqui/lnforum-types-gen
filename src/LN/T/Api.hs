{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Api where





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

data ApiRequest = ApiRequest {
  apiRequestComment :: !((Maybe Text)),
  apiRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ApiRequest where
  parseJSON (Object o) = do
    apiRequestComment <- o .: ("comment")
    apiRequestGuard <- o .: ("guard")
    pure $ ApiRequest {
      apiRequestComment = apiRequestComment,
      apiRequestGuard = apiRequestGuard
    }
  parseJSON x = fail $ "ApiRequest: Could not parse object: " <> show x


instance ToJSON ApiRequest where
  toJSON ApiRequest{..} = object $
    [ "tag" .= ("ApiRequest" :: Text)
    , "comment" .= apiRequestComment
    , "guard" .= apiRequestGuard
    ]


instance Eq ApiRequest where
  (==) a b = apiRequestComment a == apiRequestComment b && apiRequestGuard a == apiRequestGuard b

instance Show ApiRequest where
    show rec = "apiRequestComment: " <> show (apiRequestComment rec) <> ", " <> "apiRequestGuard: " <> show (apiRequestGuard rec)

data ApiResponse = ApiResponse {
  apiResponseId :: !(Int64),
  apiResponseUserId :: !(Int64),
  apiResponseKey :: !(Text),
  apiResponseComment :: !((Maybe Text)),
  apiResponseGuard :: !(Int),
  apiResponseCreatedAt :: !((Maybe UTCTime)),
  apiResponseModifiedAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ApiResponse where
  parseJSON (Object o) = do
    apiResponseId <- o .: ("id")
    apiResponseUserId <- o .: ("user_id")
    apiResponseKey <- o .: ("key")
    apiResponseComment <- o .: ("comment")
    apiResponseGuard <- o .: ("guard")
    apiResponseCreatedAt <- o .: ("created_at")
    apiResponseModifiedAt <- o .: ("modified_at")
    pure $ ApiResponse {
      apiResponseId = apiResponseId,
      apiResponseUserId = apiResponseUserId,
      apiResponseKey = apiResponseKey,
      apiResponseComment = apiResponseComment,
      apiResponseGuard = apiResponseGuard,
      apiResponseCreatedAt = apiResponseCreatedAt,
      apiResponseModifiedAt = apiResponseModifiedAt
    }
  parseJSON x = fail $ "ApiResponse: Could not parse object: " <> show x


instance ToJSON ApiResponse where
  toJSON ApiResponse{..} = object $
    [ "tag" .= ("ApiResponse" :: Text)
    , "id" .= apiResponseId
    , "user_id" .= apiResponseUserId
    , "key" .= apiResponseKey
    , "comment" .= apiResponseComment
    , "guard" .= apiResponseGuard
    , "created_at" .= apiResponseCreatedAt
    , "modified_at" .= apiResponseModifiedAt
    ]


instance Eq ApiResponse where
  (==) a b = apiResponseId a == apiResponseId b && apiResponseUserId a == apiResponseUserId b && apiResponseKey a == apiResponseKey b && apiResponseComment a == apiResponseComment b && apiResponseGuard a == apiResponseGuard b && apiResponseCreatedAt a == apiResponseCreatedAt b && apiResponseModifiedAt a == apiResponseModifiedAt b

instance Show ApiResponse where
    show rec = "apiResponseId: " <> show (apiResponseId rec) <> ", " <> "apiResponseUserId: " <> show (apiResponseUserId rec) <> ", " <> "apiResponseKey: " <> show (apiResponseKey rec) <> ", " <> "apiResponseComment: " <> show (apiResponseComment rec) <> ", " <> "apiResponseGuard: " <> show (apiResponseGuard rec) <> ", " <> "apiResponseCreatedAt: " <> show (apiResponseCreatedAt rec) <> ", " <> "apiResponseModifiedAt: " <> show (apiResponseModifiedAt rec)

data ApiResponses = ApiResponses {
  apiResponses :: !([ApiResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ApiResponses where
  parseJSON (Object o) = do
    apiResponses <- o .: ("api_responses")
    pure $ ApiResponses {
      apiResponses = apiResponses
    }
  parseJSON x = fail $ "ApiResponses: Could not parse object: " <> show x


instance ToJSON ApiResponses where
  toJSON ApiResponses{..} = object $
    [ "tag" .= ("ApiResponses" :: Text)
    , "api_responses" .= apiResponses
    ]


instance Eq ApiResponses where
  (==) a b = apiResponses a == apiResponses b

instance Show ApiResponses where
    show rec = "apiResponses: " <> show (apiResponses rec)
-- footer
