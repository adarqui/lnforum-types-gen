{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.ThreadPost where


import LN.T.ThreadPost
import LN.T.User
import LN.T.Permission
import LN.T.Like
import LN.T.Board
import LN.T.Thread


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

data ThreadPostPackResponse = ThreadPostPackResponse {
  threadPostPackResponseThreadPost :: !(ThreadPostResponse),
  threadPostPackResponseThreadPostId :: !(Int64),
  threadPostPackResponseUser :: !(UserSanitizedResponse),
  threadPostPackResponseUserId :: !(Int64),
  threadPostPackResponseStat :: !(ThreadPostStatResponse),
  threadPostPackResponseLike :: !((Maybe LikeResponse)),
  threadPostPackResponseWithBoard :: !((Maybe BoardResponse)),
  threadPostPackResponseWithThread :: !((Maybe ThreadResponse)),
  threadPostPackResponseWithThreadPosts :: !((Maybe [Int64])),
  threadPostPackResponseWithThreadPostsOffset :: !((Maybe Int64)),
  threadPostPackResponseWithThreadPostsLimit :: !((Maybe Int64)),
  threadPostPackResponsePermissions :: !(Permissions)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostPackResponse where
  parseJSON (Object o) = do
    threadPostPackResponseThreadPost <- o .: ("thread_post")
    threadPostPackResponseThreadPostId <- o .: ("thread_post_id")
    threadPostPackResponseUser <- o .: ("user")
    threadPostPackResponseUserId <- o .: ("user_id")
    threadPostPackResponseStat <- o .: ("stat")
    threadPostPackResponseLike <- o .: ("like")
    threadPostPackResponseWithBoard <- o .: ("with_board")
    threadPostPackResponseWithThread <- o .: ("with_thread")
    threadPostPackResponseWithThreadPosts <- o .: ("with_thread_posts")
    threadPostPackResponseWithThreadPostsOffset <- o .: ("with_thread_posts_offset")
    threadPostPackResponseWithThreadPostsLimit <- o .: ("with_thread_posts_limit")
    threadPostPackResponsePermissions <- o .: ("permissions")
    pure $ ThreadPostPackResponse {
      threadPostPackResponseThreadPost = threadPostPackResponseThreadPost,
      threadPostPackResponseThreadPostId = threadPostPackResponseThreadPostId,
      threadPostPackResponseUser = threadPostPackResponseUser,
      threadPostPackResponseUserId = threadPostPackResponseUserId,
      threadPostPackResponseStat = threadPostPackResponseStat,
      threadPostPackResponseLike = threadPostPackResponseLike,
      threadPostPackResponseWithBoard = threadPostPackResponseWithBoard,
      threadPostPackResponseWithThread = threadPostPackResponseWithThread,
      threadPostPackResponseWithThreadPosts = threadPostPackResponseWithThreadPosts,
      threadPostPackResponseWithThreadPostsOffset = threadPostPackResponseWithThreadPostsOffset,
      threadPostPackResponseWithThreadPostsLimit = threadPostPackResponseWithThreadPostsLimit,
      threadPostPackResponsePermissions = threadPostPackResponsePermissions
    }
  parseJSON x = fail $ "ThreadPostPackResponse: Could not parse object: " <> show x


instance ToJSON ThreadPostPackResponse where
  toJSON ThreadPostPackResponse{..} = object $
    [ "tag" .= ("ThreadPostPackResponse" :: Text)
    , "thread_post" .= threadPostPackResponseThreadPost
    , "thread_post_id" .= threadPostPackResponseThreadPostId
    , "user" .= threadPostPackResponseUser
    , "user_id" .= threadPostPackResponseUserId
    , "stat" .= threadPostPackResponseStat
    , "like" .= threadPostPackResponseLike
    , "with_board" .= threadPostPackResponseWithBoard
    , "with_thread" .= threadPostPackResponseWithThread
    , "with_thread_posts" .= threadPostPackResponseWithThreadPosts
    , "with_thread_posts_offset" .= threadPostPackResponseWithThreadPostsOffset
    , "with_thread_posts_limit" .= threadPostPackResponseWithThreadPostsLimit
    , "permissions" .= threadPostPackResponsePermissions
    ]


instance Eq ThreadPostPackResponse where
  (==) a b = threadPostPackResponseThreadPost a == threadPostPackResponseThreadPost b && threadPostPackResponseThreadPostId a == threadPostPackResponseThreadPostId b && threadPostPackResponseUser a == threadPostPackResponseUser b && threadPostPackResponseUserId a == threadPostPackResponseUserId b && threadPostPackResponseStat a == threadPostPackResponseStat b && threadPostPackResponseLike a == threadPostPackResponseLike b && threadPostPackResponseWithBoard a == threadPostPackResponseWithBoard b && threadPostPackResponseWithThread a == threadPostPackResponseWithThread b && threadPostPackResponseWithThreadPosts a == threadPostPackResponseWithThreadPosts b && threadPostPackResponseWithThreadPostsOffset a == threadPostPackResponseWithThreadPostsOffset b && threadPostPackResponseWithThreadPostsLimit a == threadPostPackResponseWithThreadPostsLimit b && threadPostPackResponsePermissions a == threadPostPackResponsePermissions b

instance Show ThreadPostPackResponse where
    show rec = "threadPostPackResponseThreadPost: " <> show (threadPostPackResponseThreadPost rec) <> ", " <> "threadPostPackResponseThreadPostId: " <> show (threadPostPackResponseThreadPostId rec) <> ", " <> "threadPostPackResponseUser: " <> show (threadPostPackResponseUser rec) <> ", " <> "threadPostPackResponseUserId: " <> show (threadPostPackResponseUserId rec) <> ", " <> "threadPostPackResponseStat: " <> show (threadPostPackResponseStat rec) <> ", " <> "threadPostPackResponseLike: " <> show (threadPostPackResponseLike rec) <> ", " <> "threadPostPackResponseWithBoard: " <> show (threadPostPackResponseWithBoard rec) <> ", " <> "threadPostPackResponseWithThread: " <> show (threadPostPackResponseWithThread rec) <> ", " <> "threadPostPackResponseWithThreadPosts: " <> show (threadPostPackResponseWithThreadPosts rec) <> ", " <> "threadPostPackResponseWithThreadPostsOffset: " <> show (threadPostPackResponseWithThreadPostsOffset rec) <> ", " <> "threadPostPackResponseWithThreadPostsLimit: " <> show (threadPostPackResponseWithThreadPostsLimit rec) <> ", " <> "threadPostPackResponsePermissions: " <> show (threadPostPackResponsePermissions rec)

data ThreadPostPackResponses = ThreadPostPackResponses {
  threadPostPackResponses :: !([ThreadPostPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostPackResponses where
  parseJSON (Object o) = do
    threadPostPackResponses <- o .: ("thread_post_pack_responses")
    pure $ ThreadPostPackResponses {
      threadPostPackResponses = threadPostPackResponses
    }
  parseJSON x = fail $ "ThreadPostPackResponses: Could not parse object: " <> show x


instance ToJSON ThreadPostPackResponses where
  toJSON ThreadPostPackResponses{..} = object $
    [ "tag" .= ("ThreadPostPackResponses" :: Text)
    , "thread_post_pack_responses" .= threadPostPackResponses
    ]


instance Eq ThreadPostPackResponses where
  (==) a b = threadPostPackResponses a == threadPostPackResponses b

instance Show ThreadPostPackResponses where
    show rec = "threadPostPackResponses: " <> show (threadPostPackResponses rec)
-- footer
