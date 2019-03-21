{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.ThreadPost where





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

data ThreadPostRequest = ThreadPostRequest {
  threadPostRequestTitle :: !((Maybe Text)),
  threadPostRequestBody :: !(PostData),
  threadPostRequestTags :: !([Text]),
  threadPostRequestPrivateTags :: !([Text]),
  threadPostRequestGuard :: !(Int),
  threadPostRequestStateTag :: !((Maybe Text)),
  threadPostRequestStatePrivateTag :: !((Maybe Text))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostRequest where
  parseJSON (Object o) = do
    threadPostRequestTitle <- o .: ("title" :: Text)
    threadPostRequestBody <- o .: ("body" :: Text)
    threadPostRequestTags <- o .: ("tags" :: Text)
    threadPostRequestPrivateTags <- o .: ("private_tags" :: Text)
    threadPostRequestGuard <- o .: ("guard" :: Text)
    threadPostRequestStateTag <- o .: ("state_tag" :: Text)
    threadPostRequestStatePrivateTag <- o .: ("state_private_tag" :: Text)
    pure $ ThreadPostRequest {
      threadPostRequestTitle = threadPostRequestTitle,
      threadPostRequestBody = threadPostRequestBody,
      threadPostRequestTags = threadPostRequestTags,
      threadPostRequestPrivateTags = threadPostRequestPrivateTags,
      threadPostRequestGuard = threadPostRequestGuard,
      threadPostRequestStateTag = threadPostRequestStateTag,
      threadPostRequestStatePrivateTag = threadPostRequestStatePrivateTag
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPostRequest where
  toJSON ThreadPostRequest{..} = object $
    [ "tag" .= ("ThreadPostRequest" :: Text)
    , "title" .= threadPostRequestTitle
    , "body" .= threadPostRequestBody
    , "tags" .= threadPostRequestTags
    , "private_tags" .= threadPostRequestPrivateTags
    , "guard" .= threadPostRequestGuard
    , "state_tag" .= threadPostRequestStateTag
    , "state_private_tag" .= threadPostRequestStatePrivateTag
    ]


instance Eq ThreadPostRequest where
  (==) a b = threadPostRequestTitle a == threadPostRequestTitle b && threadPostRequestBody a == threadPostRequestBody b && threadPostRequestTags a == threadPostRequestTags b && threadPostRequestPrivateTags a == threadPostRequestPrivateTags b && threadPostRequestGuard a == threadPostRequestGuard b && threadPostRequestStateTag a == threadPostRequestStateTag b && threadPostRequestStatePrivateTag a == threadPostRequestStatePrivateTag b

instance Show ThreadPostRequest where
    show rec = "threadPostRequestTitle: " <> show (threadPostRequestTitle rec) <> ", " <> "threadPostRequestBody: " <> show (threadPostRequestBody rec) <> ", " <> "threadPostRequestTags: " <> show (threadPostRequestTags rec) <> ", " <> "threadPostRequestPrivateTags: " <> show (threadPostRequestPrivateTags rec) <> ", " <> "threadPostRequestGuard: " <> show (threadPostRequestGuard rec) <> ", " <> "threadPostRequestStateTag: " <> show (threadPostRequestStateTag rec) <> ", " <> "threadPostRequestStatePrivateTag: " <> show (threadPostRequestStatePrivateTag rec)

data ThreadPostResponse = ThreadPostResponse {
  threadPostResponseId :: !(Int64),
  threadPostResponseUserId :: !(Int64),
  threadPostResponseOrgId :: !(Int64),
  threadPostResponseForumId :: !(Int64),
  threadPostResponseBoardId :: !(Int64),
  threadPostResponseThreadId :: !(Int64),
  threadPostResponseParentId :: !((Maybe Int64)),
  threadPostResponseTitle :: !((Maybe Text)),
  threadPostResponseBody :: !(PostData),
  threadPostResponseTags :: !([Text]),
  threadPostResponsePrivateTags :: !([Text]),
  threadPostResponseActive :: !(Bool),
  threadPostResponseGuard :: !(Int),
  threadPostResponseCreatedAt :: !((Maybe UTCTime)),
  threadPostResponseModifiedBy :: !((Maybe Int64)),
  threadPostResponseModifiedAt :: !((Maybe UTCTime)),
  threadPostResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostResponse where
  parseJSON (Object o) = do
    threadPostResponseId <- o .: ("id" :: Text)
    threadPostResponseUserId <- o .: ("user_id" :: Text)
    threadPostResponseOrgId <- o .: ("org_id" :: Text)
    threadPostResponseForumId <- o .: ("forum_id" :: Text)
    threadPostResponseBoardId <- o .: ("board_id" :: Text)
    threadPostResponseThreadId <- o .: ("thread_id" :: Text)
    threadPostResponseParentId <- o .: ("parent_id" :: Text)
    threadPostResponseTitle <- o .: ("title" :: Text)
    threadPostResponseBody <- o .: ("body" :: Text)
    threadPostResponseTags <- o .: ("tags" :: Text)
    threadPostResponsePrivateTags <- o .: ("private_tags" :: Text)
    threadPostResponseActive <- o .: ("active" :: Text)
    threadPostResponseGuard <- o .: ("guard" :: Text)
    threadPostResponseCreatedAt <- o .: ("created_at" :: Text)
    threadPostResponseModifiedBy <- o .: ("modified_by" :: Text)
    threadPostResponseModifiedAt <- o .: ("modified_at" :: Text)
    threadPostResponseActivityAt <- o .: ("activity_at" :: Text)
    pure $ ThreadPostResponse {
      threadPostResponseId = threadPostResponseId,
      threadPostResponseUserId = threadPostResponseUserId,
      threadPostResponseOrgId = threadPostResponseOrgId,
      threadPostResponseForumId = threadPostResponseForumId,
      threadPostResponseBoardId = threadPostResponseBoardId,
      threadPostResponseThreadId = threadPostResponseThreadId,
      threadPostResponseParentId = threadPostResponseParentId,
      threadPostResponseTitle = threadPostResponseTitle,
      threadPostResponseBody = threadPostResponseBody,
      threadPostResponseTags = threadPostResponseTags,
      threadPostResponsePrivateTags = threadPostResponsePrivateTags,
      threadPostResponseActive = threadPostResponseActive,
      threadPostResponseGuard = threadPostResponseGuard,
      threadPostResponseCreatedAt = threadPostResponseCreatedAt,
      threadPostResponseModifiedBy = threadPostResponseModifiedBy,
      threadPostResponseModifiedAt = threadPostResponseModifiedAt,
      threadPostResponseActivityAt = threadPostResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPostResponse where
  toJSON ThreadPostResponse{..} = object $
    [ "tag" .= ("ThreadPostResponse" :: Text)
    , "id" .= threadPostResponseId
    , "user_id" .= threadPostResponseUserId
    , "org_id" .= threadPostResponseOrgId
    , "forum_id" .= threadPostResponseForumId
    , "board_id" .= threadPostResponseBoardId
    , "thread_id" .= threadPostResponseThreadId
    , "parent_id" .= threadPostResponseParentId
    , "title" .= threadPostResponseTitle
    , "body" .= threadPostResponseBody
    , "tags" .= threadPostResponseTags
    , "private_tags" .= threadPostResponsePrivateTags
    , "active" .= threadPostResponseActive
    , "guard" .= threadPostResponseGuard
    , "created_at" .= threadPostResponseCreatedAt
    , "modified_by" .= threadPostResponseModifiedBy
    , "modified_at" .= threadPostResponseModifiedAt
    , "activity_at" .= threadPostResponseActivityAt
    ]


instance Eq ThreadPostResponse where
  (==) a b = threadPostResponseId a == threadPostResponseId b && threadPostResponseUserId a == threadPostResponseUserId b && threadPostResponseOrgId a == threadPostResponseOrgId b && threadPostResponseForumId a == threadPostResponseForumId b && threadPostResponseBoardId a == threadPostResponseBoardId b && threadPostResponseThreadId a == threadPostResponseThreadId b && threadPostResponseParentId a == threadPostResponseParentId b && threadPostResponseTitle a == threadPostResponseTitle b && threadPostResponseBody a == threadPostResponseBody b && threadPostResponseTags a == threadPostResponseTags b && threadPostResponsePrivateTags a == threadPostResponsePrivateTags b && threadPostResponseActive a == threadPostResponseActive b && threadPostResponseGuard a == threadPostResponseGuard b && threadPostResponseCreatedAt a == threadPostResponseCreatedAt b && threadPostResponseModifiedBy a == threadPostResponseModifiedBy b && threadPostResponseModifiedAt a == threadPostResponseModifiedAt b && threadPostResponseActivityAt a == threadPostResponseActivityAt b

instance Show ThreadPostResponse where
    show rec = "threadPostResponseId: " <> show (threadPostResponseId rec) <> ", " <> "threadPostResponseUserId: " <> show (threadPostResponseUserId rec) <> ", " <> "threadPostResponseOrgId: " <> show (threadPostResponseOrgId rec) <> ", " <> "threadPostResponseForumId: " <> show (threadPostResponseForumId rec) <> ", " <> "threadPostResponseBoardId: " <> show (threadPostResponseBoardId rec) <> ", " <> "threadPostResponseThreadId: " <> show (threadPostResponseThreadId rec) <> ", " <> "threadPostResponseParentId: " <> show (threadPostResponseParentId rec) <> ", " <> "threadPostResponseTitle: " <> show (threadPostResponseTitle rec) <> ", " <> "threadPostResponseBody: " <> show (threadPostResponseBody rec) <> ", " <> "threadPostResponseTags: " <> show (threadPostResponseTags rec) <> ", " <> "threadPostResponsePrivateTags: " <> show (threadPostResponsePrivateTags rec) <> ", " <> "threadPostResponseActive: " <> show (threadPostResponseActive rec) <> ", " <> "threadPostResponseGuard: " <> show (threadPostResponseGuard rec) <> ", " <> "threadPostResponseCreatedAt: " <> show (threadPostResponseCreatedAt rec) <> ", " <> "threadPostResponseModifiedBy: " <> show (threadPostResponseModifiedBy rec) <> ", " <> "threadPostResponseModifiedAt: " <> show (threadPostResponseModifiedAt rec) <> ", " <> "threadPostResponseActivityAt: " <> show (threadPostResponseActivityAt rec)

data ThreadPostResponses = ThreadPostResponses {
  threadPostResponses :: !([ThreadPostResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostResponses where
  parseJSON (Object o) = do
    threadPostResponses <- o .: ("thread_post_responses" :: Text)
    pure $ ThreadPostResponses {
      threadPostResponses = threadPostResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ThreadPostResponses where
  toJSON ThreadPostResponses{..} = object $
    [ "tag" .= ("ThreadPostResponses" :: Text)
    , "thread_post_responses" .= threadPostResponses
    ]


instance Eq ThreadPostResponses where
  (==) a b = threadPostResponses a == threadPostResponses b

instance Show ThreadPostResponses where
    show rec = "threadPostResponses: " <> show (threadPostResponses rec)
-- footer