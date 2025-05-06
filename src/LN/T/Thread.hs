{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Thread where


import LN.T.Board


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

data ThreadRequest = ThreadRequest {
  threadRequestDisplayName :: !(Text),
  threadRequestDescription :: !((Maybe Text)),
  threadRequestSticky :: !(Bool),
  threadRequestLocked :: !(Bool),
  threadRequestPoll :: !((Maybe Text)),
  threadRequestIcon :: !((Maybe Text)),
  threadRequestTags :: !([Text]),
  threadRequestGuard :: !(Int),
  threadRequestStateTag :: !((Maybe Text))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadRequest where
  parseJSON (Object o) = do
    threadRequestDisplayName <- o .: ("display_name" :: Data.Aeson.Key.Key)
    threadRequestDescription <- o .: ("description" :: Data.Aeson.Key.Key)
    threadRequestSticky <- o .: ("sticky" :: Data.Aeson.Key.Key)
    threadRequestLocked <- o .: ("locked" :: Data.Aeson.Key.Key)
    threadRequestPoll <- o .: ("poll" :: Data.Aeson.Key.Key)
    threadRequestIcon <- o .: ("icon" :: Data.Aeson.Key.Key)
    threadRequestTags <- o .: ("tags" :: Data.Aeson.Key.Key)
    threadRequestGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    threadRequestStateTag <- o .: ("state_tag" :: Data.Aeson.Key.Key)
    pure $ ThreadRequest {
      threadRequestDisplayName = threadRequestDisplayName,
      threadRequestDescription = threadRequestDescription,
      threadRequestSticky = threadRequestSticky,
      threadRequestLocked = threadRequestLocked,
      threadRequestPoll = threadRequestPoll,
      threadRequestIcon = threadRequestIcon,
      threadRequestTags = threadRequestTags,
      threadRequestGuard = threadRequestGuard,
      threadRequestStateTag = threadRequestStateTag
    }
  parseJSON x = fail $ "ThreadRequest: Could not parse object: " <> show x


instance ToJSON ThreadRequest where
  toJSON ThreadRequest{..} = object $
    [ "tag" .= ("ThreadRequest" :: Text)
    , "display_name" .= threadRequestDisplayName
    , "description" .= threadRequestDescription
    , "sticky" .= threadRequestSticky
    , "locked" .= threadRequestLocked
    , "poll" .= threadRequestPoll
    , "icon" .= threadRequestIcon
    , "tags" .= threadRequestTags
    , "guard" .= threadRequestGuard
    , "state_tag" .= threadRequestStateTag
    ]


instance Eq ThreadRequest where
  (==) a b = threadRequestDisplayName a == threadRequestDisplayName b && threadRequestDescription a == threadRequestDescription b && threadRequestSticky a == threadRequestSticky b && threadRequestLocked a == threadRequestLocked b && threadRequestPoll a == threadRequestPoll b && threadRequestIcon a == threadRequestIcon b && threadRequestTags a == threadRequestTags b && threadRequestGuard a == threadRequestGuard b && threadRequestStateTag a == threadRequestStateTag b

instance Show ThreadRequest where
    show rec = "threadRequestDisplayName: " <> show (threadRequestDisplayName rec) <> ", " <> "threadRequestDescription: " <> show (threadRequestDescription rec) <> ", " <> "threadRequestSticky: " <> show (threadRequestSticky rec) <> ", " <> "threadRequestLocked: " <> show (threadRequestLocked rec) <> ", " <> "threadRequestPoll: " <> show (threadRequestPoll rec) <> ", " <> "threadRequestIcon: " <> show (threadRequestIcon rec) <> ", " <> "threadRequestTags: " <> show (threadRequestTags rec) <> ", " <> "threadRequestGuard: " <> show (threadRequestGuard rec) <> ", " <> "threadRequestStateTag: " <> show (threadRequestStateTag rec)

data ThreadResponse = ThreadResponse {
  threadResponseId :: !(Int64),
  threadResponseUserId :: !(Int64),
  threadResponseBoardId :: !(Int64),
  threadResponseName :: !(Text),
  threadResponseDisplayName :: !(Text),
  threadResponseDescription :: !((Maybe Text)),
  threadResponseSticky :: !(Bool),
  threadResponseLocked :: !(Bool),
  threadResponsePoll :: !((Maybe Text)),
  threadResponseIcon :: !((Maybe Text)),
  threadResponseTags :: !([Text]),
  threadResponseActive :: !(Bool),
  threadResponseGuard :: !(Int),
  threadResponseCreatedAt :: !((Maybe UTCTime)),
  threadResponseModifiedBy :: !((Maybe Int64)),
  threadResponseModifiedAt :: !((Maybe UTCTime)),
  threadResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadResponse where
  parseJSON (Object o) = do
    threadResponseId <- o .: ("id" :: Data.Aeson.Key.Key)
    threadResponseUserId <- o .: ("user_id" :: Data.Aeson.Key.Key)
    threadResponseBoardId <- o .: ("board_id" :: Data.Aeson.Key.Key)
    threadResponseName <- o .: ("name" :: Data.Aeson.Key.Key)
    threadResponseDisplayName <- o .: ("display_name" :: Data.Aeson.Key.Key)
    threadResponseDescription <- o .: ("description" :: Data.Aeson.Key.Key)
    threadResponseSticky <- o .: ("sticky" :: Data.Aeson.Key.Key)
    threadResponseLocked <- o .: ("locked" :: Data.Aeson.Key.Key)
    threadResponsePoll <- o .: ("poll" :: Data.Aeson.Key.Key)
    threadResponseIcon <- o .: ("icon" :: Data.Aeson.Key.Key)
    threadResponseTags <- o .: ("tags" :: Data.Aeson.Key.Key)
    threadResponseActive <- o .: ("active" :: Data.Aeson.Key.Key)
    threadResponseGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    threadResponseCreatedAt <- o .: ("created_at" :: Data.Aeson.Key.Key)
    threadResponseModifiedBy <- o .: ("modified_by" :: Data.Aeson.Key.Key)
    threadResponseModifiedAt <- o .: ("modified_at" :: Data.Aeson.Key.Key)
    threadResponseActivityAt <- o .: ("activity_at" :: Data.Aeson.Key.Key)
    pure $ ThreadResponse {
      threadResponseId = threadResponseId,
      threadResponseUserId = threadResponseUserId,
      threadResponseBoardId = threadResponseBoardId,
      threadResponseName = threadResponseName,
      threadResponseDisplayName = threadResponseDisplayName,
      threadResponseDescription = threadResponseDescription,
      threadResponseSticky = threadResponseSticky,
      threadResponseLocked = threadResponseLocked,
      threadResponsePoll = threadResponsePoll,
      threadResponseIcon = threadResponseIcon,
      threadResponseTags = threadResponseTags,
      threadResponseActive = threadResponseActive,
      threadResponseGuard = threadResponseGuard,
      threadResponseCreatedAt = threadResponseCreatedAt,
      threadResponseModifiedBy = threadResponseModifiedBy,
      threadResponseModifiedAt = threadResponseModifiedAt,
      threadResponseActivityAt = threadResponseActivityAt
    }
  parseJSON x = fail $ "ThreadResponse: Could not parse object: " <> show x


instance ToJSON ThreadResponse where
  toJSON ThreadResponse{..} = object $
    [ "tag" .= ("ThreadResponse" :: Text)
    , "id" .= threadResponseId
    , "user_id" .= threadResponseUserId
    , "board_id" .= threadResponseBoardId
    , "name" .= threadResponseName
    , "display_name" .= threadResponseDisplayName
    , "description" .= threadResponseDescription
    , "sticky" .= threadResponseSticky
    , "locked" .= threadResponseLocked
    , "poll" .= threadResponsePoll
    , "icon" .= threadResponseIcon
    , "tags" .= threadResponseTags
    , "active" .= threadResponseActive
    , "guard" .= threadResponseGuard
    , "created_at" .= threadResponseCreatedAt
    , "modified_by" .= threadResponseModifiedBy
    , "modified_at" .= threadResponseModifiedAt
    , "activity_at" .= threadResponseActivityAt
    ]


instance Eq ThreadResponse where
  (==) a b = threadResponseId a == threadResponseId b && threadResponseUserId a == threadResponseUserId b && threadResponseBoardId a == threadResponseBoardId b && threadResponseName a == threadResponseName b && threadResponseDisplayName a == threadResponseDisplayName b && threadResponseDescription a == threadResponseDescription b && threadResponseSticky a == threadResponseSticky b && threadResponseLocked a == threadResponseLocked b && threadResponsePoll a == threadResponsePoll b && threadResponseIcon a == threadResponseIcon b && threadResponseTags a == threadResponseTags b && threadResponseActive a == threadResponseActive b && threadResponseGuard a == threadResponseGuard b && threadResponseCreatedAt a == threadResponseCreatedAt b && threadResponseModifiedBy a == threadResponseModifiedBy b && threadResponseModifiedAt a == threadResponseModifiedAt b && threadResponseActivityAt a == threadResponseActivityAt b

instance Show ThreadResponse where
    show rec = "threadResponseId: " <> show (threadResponseId rec) <> ", " <> "threadResponseUserId: " <> show (threadResponseUserId rec) <> ", " <> "threadResponseBoardId: " <> show (threadResponseBoardId rec) <> ", " <> "threadResponseName: " <> show (threadResponseName rec) <> ", " <> "threadResponseDisplayName: " <> show (threadResponseDisplayName rec) <> ", " <> "threadResponseDescription: " <> show (threadResponseDescription rec) <> ", " <> "threadResponseSticky: " <> show (threadResponseSticky rec) <> ", " <> "threadResponseLocked: " <> show (threadResponseLocked rec) <> ", " <> "threadResponsePoll: " <> show (threadResponsePoll rec) <> ", " <> "threadResponseIcon: " <> show (threadResponseIcon rec) <> ", " <> "threadResponseTags: " <> show (threadResponseTags rec) <> ", " <> "threadResponseActive: " <> show (threadResponseActive rec) <> ", " <> "threadResponseGuard: " <> show (threadResponseGuard rec) <> ", " <> "threadResponseCreatedAt: " <> show (threadResponseCreatedAt rec) <> ", " <> "threadResponseModifiedBy: " <> show (threadResponseModifiedBy rec) <> ", " <> "threadResponseModifiedAt: " <> show (threadResponseModifiedAt rec) <> ", " <> "threadResponseActivityAt: " <> show (threadResponseActivityAt rec)

data ThreadResponses = ThreadResponses {
  threadResponses :: !([ThreadResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadResponses where
  parseJSON (Object o) = do
    threadResponses <- o .: ("thread_responses" :: Data.Aeson.Key.Key)
    pure $ ThreadResponses {
      threadResponses = threadResponses
    }
  parseJSON x = fail $ "ThreadResponses: Could not parse object: " <> show x


instance ToJSON ThreadResponses where
  toJSON ThreadResponses{..} = object $
    [ "tag" .= ("ThreadResponses" :: Text)
    , "thread_responses" .= threadResponses
    ]


instance Eq ThreadResponses where
  (==) a b = threadResponses a == threadResponses b

instance Show ThreadResponses where
    show rec = "threadResponses: " <> show (threadResponses rec)

data ThreadStatResponse = ThreadStatResponse {
  threadStatResponseThreadId :: !(Int64),
  threadStatResponseThreadPosts :: !(Int64),
  threadStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadStatResponse where
  parseJSON (Object o) = do
    threadStatResponseThreadId <- o .: ("thread_id" :: Data.Aeson.Key.Key)
    threadStatResponseThreadPosts <- o .: ("thread_posts" :: Data.Aeson.Key.Key)
    threadStatResponseViews <- o .: ("views" :: Data.Aeson.Key.Key)
    pure $ ThreadStatResponse {
      threadStatResponseThreadId = threadStatResponseThreadId,
      threadStatResponseThreadPosts = threadStatResponseThreadPosts,
      threadStatResponseViews = threadStatResponseViews
    }
  parseJSON x = fail $ "ThreadStatResponse: Could not parse object: " <> show x


instance ToJSON ThreadStatResponse where
  toJSON ThreadStatResponse{..} = object $
    [ "tag" .= ("ThreadStatResponse" :: Text)
    , "thread_id" .= threadStatResponseThreadId
    , "thread_posts" .= threadStatResponseThreadPosts
    , "views" .= threadStatResponseViews
    ]


instance Eq ThreadStatResponse where
  (==) a b = threadStatResponseThreadId a == threadStatResponseThreadId b && threadStatResponseThreadPosts a == threadStatResponseThreadPosts b && threadStatResponseViews a == threadStatResponseViews b

instance Show ThreadStatResponse where
    show rec = "threadStatResponseThreadId: " <> show (threadStatResponseThreadId rec) <> ", " <> "threadStatResponseThreadPosts: " <> show (threadStatResponseThreadPosts rec) <> ", " <> "threadStatResponseViews: " <> show (threadStatResponseViews rec)

data ThreadStatResponses = ThreadStatResponses {
  threadStatResponses :: !([ThreadStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadStatResponses where
  parseJSON (Object o) = do
    threadStatResponses <- o .: ("thread_stat_responses" :: Data.Aeson.Key.Key)
    pure $ ThreadStatResponses {
      threadStatResponses = threadStatResponses
    }
  parseJSON x = fail $ "ThreadStatResponses: Could not parse object: " <> show x


instance ToJSON ThreadStatResponses where
  toJSON ThreadStatResponses{..} = object $
    [ "tag" .= ("ThreadStatResponses" :: Text)
    , "thread_stat_responses" .= threadStatResponses
    ]


instance Eq ThreadStatResponses where
  (==) a b = threadStatResponses a == threadStatResponses b

instance Show ThreadStatResponses where
    show rec = "threadStatResponses: " <> show (threadStatResponses rec)
-- footer
