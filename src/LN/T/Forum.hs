{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Forum where


import LN.T.Visibility


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

data ForumRequest = ForumRequest {
  forumRequestDisplayName :: !(Text),
  forumRequestDescription :: !((Maybe Text)),
  forumRequestThreadsPerBoard :: !(Int),
  forumRequestThreadPostsPerThread :: !(Int),
  forumRequestRecentThreadsLimit :: !(Int),
  forumRequestRecentPostsLimit :: !(Int),
  forumRequestMotwLimit :: !(Int),
  forumRequestIcon :: !((Maybe Text)),
  forumRequestTags :: !([Text]),
  forumRequestVisibility :: !(Visibility),
  forumRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ForumRequest where
  parseJSON (Object o) = do
    forumRequestDisplayName <- o .: ("display_name")
    forumRequestDescription <- o .: ("description")
    forumRequestThreadsPerBoard <- o .: ("threads_per_board")
    forumRequestThreadPostsPerThread <- o .: ("thread_posts_per_thread")
    forumRequestRecentThreadsLimit <- o .: ("recent_threads_limit")
    forumRequestRecentPostsLimit <- o .: ("recent_posts_limit")
    forumRequestMotwLimit <- o .: ("motw_limit")
    forumRequestIcon <- o .: ("icon")
    forumRequestTags <- o .: ("tags")
    forumRequestVisibility <- o .: ("visibility")
    forumRequestGuard <- o .: ("guard")
    pure $ ForumRequest {
      forumRequestDisplayName = forumRequestDisplayName,
      forumRequestDescription = forumRequestDescription,
      forumRequestThreadsPerBoard = forumRequestThreadsPerBoard,
      forumRequestThreadPostsPerThread = forumRequestThreadPostsPerThread,
      forumRequestRecentThreadsLimit = forumRequestRecentThreadsLimit,
      forumRequestRecentPostsLimit = forumRequestRecentPostsLimit,
      forumRequestMotwLimit = forumRequestMotwLimit,
      forumRequestIcon = forumRequestIcon,
      forumRequestTags = forumRequestTags,
      forumRequestVisibility = forumRequestVisibility,
      forumRequestGuard = forumRequestGuard
    }
  parseJSON x = fail $ "ForumRequest: Could not parse object: " <> show x


instance ToJSON ForumRequest where
  toJSON ForumRequest{..} = object $
    [ "tag" .= ("ForumRequest" :: Text)
    , "display_name" .= forumRequestDisplayName
    , "description" .= forumRequestDescription
    , "threads_per_board" .= forumRequestThreadsPerBoard
    , "thread_posts_per_thread" .= forumRequestThreadPostsPerThread
    , "recent_threads_limit" .= forumRequestRecentThreadsLimit
    , "recent_posts_limit" .= forumRequestRecentPostsLimit
    , "motw_limit" .= forumRequestMotwLimit
    , "icon" .= forumRequestIcon
    , "tags" .= forumRequestTags
    , "visibility" .= forumRequestVisibility
    , "guard" .= forumRequestGuard
    ]


instance Eq ForumRequest where
  (==) a b = forumRequestDisplayName a == forumRequestDisplayName b && forumRequestDescription a == forumRequestDescription b && forumRequestThreadsPerBoard a == forumRequestThreadsPerBoard b && forumRequestThreadPostsPerThread a == forumRequestThreadPostsPerThread b && forumRequestRecentThreadsLimit a == forumRequestRecentThreadsLimit b && forumRequestRecentPostsLimit a == forumRequestRecentPostsLimit b && forumRequestMotwLimit a == forumRequestMotwLimit b && forumRequestIcon a == forumRequestIcon b && forumRequestTags a == forumRequestTags b && forumRequestVisibility a == forumRequestVisibility b && forumRequestGuard a == forumRequestGuard b

instance Show ForumRequest where
    show rec = "forumRequestDisplayName: " <> show (forumRequestDisplayName rec) <> ", " <> "forumRequestDescription: " <> show (forumRequestDescription rec) <> ", " <> "forumRequestThreadsPerBoard: " <> show (forumRequestThreadsPerBoard rec) <> ", " <> "forumRequestThreadPostsPerThread: " <> show (forumRequestThreadPostsPerThread rec) <> ", " <> "forumRequestRecentThreadsLimit: " <> show (forumRequestRecentThreadsLimit rec) <> ", " <> "forumRequestRecentPostsLimit: " <> show (forumRequestRecentPostsLimit rec) <> ", " <> "forumRequestMotwLimit: " <> show (forumRequestMotwLimit rec) <> ", " <> "forumRequestIcon: " <> show (forumRequestIcon rec) <> ", " <> "forumRequestTags: " <> show (forumRequestTags rec) <> ", " <> "forumRequestVisibility: " <> show (forumRequestVisibility rec) <> ", " <> "forumRequestGuard: " <> show (forumRequestGuard rec)

data ForumResponse = ForumResponse {
  forumResponseId :: !(Int64),
  forumResponseUserId :: !(Int64),
  forumResponseName :: !(Text),
  forumResponseDisplayName :: !(Text),
  forumResponseDescription :: !((Maybe Text)),
  forumResponseThreadsPerBoard :: !(Int),
  forumResponseThreadPostsPerThread :: !(Int),
  forumResponseRecentThreadsLimit :: !(Int),
  forumResponseRecentPostsLimit :: !(Int),
  forumResponseMotwLimit :: !(Int),
  forumResponseIcon :: !((Maybe Text)),
  forumResponseVisibility :: !(Visibility),
  forumResponseTags :: !([Text]),
  forumResponseGuard :: !(Int),
  forumResponseCreatedAt :: !((Maybe UTCTime)),
  forumResponseModifiedBy :: !((Maybe Int64)),
  forumResponseModifiedAt :: !((Maybe UTCTime)),
  forumResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON ForumResponse where
  parseJSON (Object o) = do
    forumResponseId <- o .: ("id")
    forumResponseUserId <- o .: ("user_id")
    forumResponseName <- o .: ("name")
    forumResponseDisplayName <- o .: ("display_name")
    forumResponseDescription <- o .: ("description")
    forumResponseThreadsPerBoard <- o .: ("threads_per_board")
    forumResponseThreadPostsPerThread <- o .: ("thread_posts_per_thread")
    forumResponseRecentThreadsLimit <- o .: ("recent_threads_limit")
    forumResponseRecentPostsLimit <- o .: ("recent_posts_limit")
    forumResponseMotwLimit <- o .: ("motw_limit")
    forumResponseIcon <- o .: ("icon")
    forumResponseVisibility <- o .: ("visibility")
    forumResponseTags <- o .: ("tags")
    forumResponseGuard <- o .: ("guard")
    forumResponseCreatedAt <- o .: ("created_at")
    forumResponseModifiedBy <- o .: ("modified_by")
    forumResponseModifiedAt <- o .: ("modified_at")
    forumResponseActivityAt <- o .: ("activity_at")
    pure $ ForumResponse {
      forumResponseId = forumResponseId,
      forumResponseUserId = forumResponseUserId,
      forumResponseName = forumResponseName,
      forumResponseDisplayName = forumResponseDisplayName,
      forumResponseDescription = forumResponseDescription,
      forumResponseThreadsPerBoard = forumResponseThreadsPerBoard,
      forumResponseThreadPostsPerThread = forumResponseThreadPostsPerThread,
      forumResponseRecentThreadsLimit = forumResponseRecentThreadsLimit,
      forumResponseRecentPostsLimit = forumResponseRecentPostsLimit,
      forumResponseMotwLimit = forumResponseMotwLimit,
      forumResponseIcon = forumResponseIcon,
      forumResponseVisibility = forumResponseVisibility,
      forumResponseTags = forumResponseTags,
      forumResponseGuard = forumResponseGuard,
      forumResponseCreatedAt = forumResponseCreatedAt,
      forumResponseModifiedBy = forumResponseModifiedBy,
      forumResponseModifiedAt = forumResponseModifiedAt,
      forumResponseActivityAt = forumResponseActivityAt
    }
  parseJSON x = fail $ "ForumResponse: Could not parse object: " <> show x


instance ToJSON ForumResponse where
  toJSON ForumResponse{..} = object $
    [ "tag" .= ("ForumResponse" :: Text)
    , "id" .= forumResponseId
    , "user_id" .= forumResponseUserId
    , "name" .= forumResponseName
    , "display_name" .= forumResponseDisplayName
    , "description" .= forumResponseDescription
    , "threads_per_board" .= forumResponseThreadsPerBoard
    , "thread_posts_per_thread" .= forumResponseThreadPostsPerThread
    , "recent_threads_limit" .= forumResponseRecentThreadsLimit
    , "recent_posts_limit" .= forumResponseRecentPostsLimit
    , "motw_limit" .= forumResponseMotwLimit
    , "icon" .= forumResponseIcon
    , "visibility" .= forumResponseVisibility
    , "tags" .= forumResponseTags
    , "guard" .= forumResponseGuard
    , "created_at" .= forumResponseCreatedAt
    , "modified_by" .= forumResponseModifiedBy
    , "modified_at" .= forumResponseModifiedAt
    , "activity_at" .= forumResponseActivityAt
    ]


instance Eq ForumResponse where
  (==) a b = forumResponseId a == forumResponseId b && forumResponseUserId a == forumResponseUserId b && forumResponseName a == forumResponseName b && forumResponseDisplayName a == forumResponseDisplayName b && forumResponseDescription a == forumResponseDescription b && forumResponseThreadsPerBoard a == forumResponseThreadsPerBoard b && forumResponseThreadPostsPerThread a == forumResponseThreadPostsPerThread b && forumResponseRecentThreadsLimit a == forumResponseRecentThreadsLimit b && forumResponseRecentPostsLimit a == forumResponseRecentPostsLimit b && forumResponseMotwLimit a == forumResponseMotwLimit b && forumResponseIcon a == forumResponseIcon b && forumResponseVisibility a == forumResponseVisibility b && forumResponseTags a == forumResponseTags b && forumResponseGuard a == forumResponseGuard b && forumResponseCreatedAt a == forumResponseCreatedAt b && forumResponseModifiedBy a == forumResponseModifiedBy b && forumResponseModifiedAt a == forumResponseModifiedAt b && forumResponseActivityAt a == forumResponseActivityAt b

instance Show ForumResponse where
    show rec = "forumResponseId: " <> show (forumResponseId rec) <> ", " <> "forumResponseUserId: " <> show (forumResponseUserId rec) <> ", " <> "forumResponseName: " <> show (forumResponseName rec) <> ", " <> "forumResponseDisplayName: " <> show (forumResponseDisplayName rec) <> ", " <> "forumResponseDescription: " <> show (forumResponseDescription rec) <> ", " <> "forumResponseThreadsPerBoard: " <> show (forumResponseThreadsPerBoard rec) <> ", " <> "forumResponseThreadPostsPerThread: " <> show (forumResponseThreadPostsPerThread rec) <> ", " <> "forumResponseRecentThreadsLimit: " <> show (forumResponseRecentThreadsLimit rec) <> ", " <> "forumResponseRecentPostsLimit: " <> show (forumResponseRecentPostsLimit rec) <> ", " <> "forumResponseMotwLimit: " <> show (forumResponseMotwLimit rec) <> ", " <> "forumResponseIcon: " <> show (forumResponseIcon rec) <> ", " <> "forumResponseVisibility: " <> show (forumResponseVisibility rec) <> ", " <> "forumResponseTags: " <> show (forumResponseTags rec) <> ", " <> "forumResponseGuard: " <> show (forumResponseGuard rec) <> ", " <> "forumResponseCreatedAt: " <> show (forumResponseCreatedAt rec) <> ", " <> "forumResponseModifiedBy: " <> show (forumResponseModifiedBy rec) <> ", " <> "forumResponseModifiedAt: " <> show (forumResponseModifiedAt rec) <> ", " <> "forumResponseActivityAt: " <> show (forumResponseActivityAt rec)

data ForumResponses = ForumResponses {
  forumResponses :: !([ForumResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ForumResponses where
  parseJSON (Object o) = do
    forumResponses <- o .: ("forum_responses")
    pure $ ForumResponses {
      forumResponses = forumResponses
    }
  parseJSON x = fail $ "ForumResponses: Could not parse object: " <> show x


instance ToJSON ForumResponses where
  toJSON ForumResponses{..} = object $
    [ "tag" .= ("ForumResponses" :: Text)
    , "forum_responses" .= forumResponses
    ]


instance Eq ForumResponses where
  (==) a b = forumResponses a == forumResponses b

instance Show ForumResponses where
    show rec = "forumResponses: " <> show (forumResponses rec)

data ForumStatResponse = ForumStatResponse {
  forumStatResponseBoards :: !(Int64),
  forumStatResponseThreads :: !(Int64),
  forumStatResponseThreadPosts :: !(Int64),
  forumStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ForumStatResponse where
  parseJSON (Object o) = do
    forumStatResponseBoards <- o .: ("boards")
    forumStatResponseThreads <- o .: ("threads")
    forumStatResponseThreadPosts <- o .: ("thread_posts")
    forumStatResponseViews <- o .: ("views")
    pure $ ForumStatResponse {
      forumStatResponseBoards = forumStatResponseBoards,
      forumStatResponseThreads = forumStatResponseThreads,
      forumStatResponseThreadPosts = forumStatResponseThreadPosts,
      forumStatResponseViews = forumStatResponseViews
    }
  parseJSON x = fail $ "ForumStatResponse: Could not parse object: " <> show x


instance ToJSON ForumStatResponse where
  toJSON ForumStatResponse{..} = object $
    [ "tag" .= ("ForumStatResponse" :: Text)
    , "boards" .= forumStatResponseBoards
    , "threads" .= forumStatResponseThreads
    , "thread_posts" .= forumStatResponseThreadPosts
    , "views" .= forumStatResponseViews
    ]


instance Eq ForumStatResponse where
  (==) a b = forumStatResponseBoards a == forumStatResponseBoards b && forumStatResponseThreads a == forumStatResponseThreads b && forumStatResponseThreadPosts a == forumStatResponseThreadPosts b && forumStatResponseViews a == forumStatResponseViews b

instance Show ForumStatResponse where
    show rec = "forumStatResponseBoards: " <> show (forumStatResponseBoards rec) <> ", " <> "forumStatResponseThreads: " <> show (forumStatResponseThreads rec) <> ", " <> "forumStatResponseThreadPosts: " <> show (forumStatResponseThreadPosts rec) <> ", " <> "forumStatResponseViews: " <> show (forumStatResponseViews rec)
-- footer
