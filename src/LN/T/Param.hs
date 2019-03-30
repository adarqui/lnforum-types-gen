{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Param where





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

data Param
  = Limit !(Int64)
  | Offset !(Int64)
  | SortOrder !(SortOrderBy)
  | Order !(OrderBy)
  | ByUserId !(Int64)
  | ByUsersIds !([Int64])
  | ByUserName !(Text)
  | ByUsersNames !([Text])
  | ByForumId !(Int64)
  | ByBoardId !(Int64)
  | ByBoardsIds !([Int64])
  | ByBoardName !(Text)
  | ByThreadId !(Int64)
  | ByThreadsIds !([Int64])
  | ByThreadName !(Text)
  | ByThreadPostId !(Int64)
  | ByThreadPostsIds !([Int64])
  | ByThreadPostName !(Text)
  | ByThreadPostLikeId !(Int64)
  | ByThreadPostLikesIds !([Int64])
  | ByPmId !(Int64)
  | ByPmsIds !([Int64])
  | ByReminderId !(Int64)
  | ByReminderFolderId !(Int64)
  | ByParentId !(Int64)
  | ByParentsIds !([Int64])
  | ByParentName !(Text)
  | ByEmail !(Text)
  | BySelf !(Bool)
  | View !(Bool)
  | Timestamp !(UTCTime)
  | UnixTimestamp !(Int64)
  | CreatedAtTimestamp !(UTCTime)
  | CreatedAtUnixTimestamp !(Int64)
  | RealIP !(Text)
  | IP !(Text)
  | WithBoard !(Bool)
  | WithThread !(Bool)
  | WithThreadPosts !(Bool)
  deriving (Generic,Typeable,NFData)


instance FromJSON Param where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("Limit" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Limit <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Limit"

      ("Offset" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Offset <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Offset"

      ("SortOrder" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> SortOrder <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: SortOrder"

      ("Order" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Order <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Order"

      ("ByUserId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUserId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUserId"

      ("ByUsersIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUsersIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUsersIds"

      ("ByUserName" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUserName <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUserName"

      ("ByUsersNames" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByUsersNames <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByUsersNames"

      ("ByForumId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByForumId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByForumId"

      ("ByBoardId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByBoardId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByBoardId"

      ("ByBoardsIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByBoardsIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByBoardsIds"

      ("ByBoardName" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByBoardName <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByBoardName"

      ("ByThreadId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadId"

      ("ByThreadsIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadsIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadsIds"

      ("ByThreadName" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadName <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadName"

      ("ByThreadPostId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadPostId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadPostId"

      ("ByThreadPostsIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadPostsIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadPostsIds"

      ("ByThreadPostName" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadPostName <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadPostName"

      ("ByThreadPostLikeId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadPostLikeId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadPostLikeId"

      ("ByThreadPostLikesIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByThreadPostLikesIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByThreadPostLikesIds"

      ("ByPmId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByPmId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByPmId"

      ("ByPmsIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByPmsIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByPmsIds"

      ("ByReminderId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByReminderId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByReminderId"

      ("ByReminderFolderId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByReminderFolderId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByReminderFolderId"

      ("ByParentId" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByParentId <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByParentId"

      ("ByParentsIds" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByParentsIds <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByParentsIds"

      ("ByParentName" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByParentName <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByParentName"

      ("ByEmail" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ByEmail <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ByEmail"

      ("BySelf" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> BySelf <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: BySelf"

      ("View" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> View <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: View"

      ("Timestamp" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> Timestamp <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: Timestamp"

      ("UnixTimestamp" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> UnixTimestamp <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: UnixTimestamp"

      ("CreatedAtTimestamp" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> CreatedAtTimestamp <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: CreatedAtTimestamp"

      ("CreatedAtUnixTimestamp" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> CreatedAtUnixTimestamp <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: CreatedAtUnixTimestamp"

      ("RealIP" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> RealIP <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: RealIP"

      ("IP" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> IP <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: IP"

      ("WithBoard" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> WithBoard <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: WithBoard"

      ("WithThread" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> WithThread <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: WithThread"

      ("WithThreadPosts" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> WithThreadPosts <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: WithThreadPosts"

      _ -> fail "Could not parse Param"

  parseJSON x = fail $ "Param: Could not parse object: " <> show x


instance ToJSON Param where
  toJSON (Limit x0) = object $
    [ "tag" .= ("Limit" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Offset x0) = object $
    [ "tag" .= ("Offset" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (SortOrder x0) = object $
    [ "tag" .= ("SortOrder" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Order x0) = object $
    [ "tag" .= ("Order" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUserId x0) = object $
    [ "tag" .= ("ByUserId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUsersIds x0) = object $
    [ "tag" .= ("ByUsersIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUserName x0) = object $
    [ "tag" .= ("ByUserName" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByUsersNames x0) = object $
    [ "tag" .= ("ByUsersNames" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByForumId x0) = object $
    [ "tag" .= ("ByForumId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByBoardId x0) = object $
    [ "tag" .= ("ByBoardId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByBoardsIds x0) = object $
    [ "tag" .= ("ByBoardsIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByBoardName x0) = object $
    [ "tag" .= ("ByBoardName" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadId x0) = object $
    [ "tag" .= ("ByThreadId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadsIds x0) = object $
    [ "tag" .= ("ByThreadsIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadName x0) = object $
    [ "tag" .= ("ByThreadName" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadPostId x0) = object $
    [ "tag" .= ("ByThreadPostId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadPostsIds x0) = object $
    [ "tag" .= ("ByThreadPostsIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadPostName x0) = object $
    [ "tag" .= ("ByThreadPostName" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadPostLikeId x0) = object $
    [ "tag" .= ("ByThreadPostLikeId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByThreadPostLikesIds x0) = object $
    [ "tag" .= ("ByThreadPostLikesIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByPmId x0) = object $
    [ "tag" .= ("ByPmId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByPmsIds x0) = object $
    [ "tag" .= ("ByPmsIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByReminderId x0) = object $
    [ "tag" .= ("ByReminderId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByReminderFolderId x0) = object $
    [ "tag" .= ("ByReminderFolderId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByParentId x0) = object $
    [ "tag" .= ("ByParentId" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByParentsIds x0) = object $
    [ "tag" .= ("ByParentsIds" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByParentName x0) = object $
    [ "tag" .= ("ByParentName" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ByEmail x0) = object $
    [ "tag" .= ("ByEmail" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (BySelf x0) = object $
    [ "tag" .= ("BySelf" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (View x0) = object $
    [ "tag" .= ("View" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (Timestamp x0) = object $
    [ "tag" .= ("Timestamp" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (UnixTimestamp x0) = object $
    [ "tag" .= ("UnixTimestamp" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (CreatedAtTimestamp x0) = object $
    [ "tag" .= ("CreatedAtTimestamp" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (CreatedAtUnixTimestamp x0) = object $
    [ "tag" .= ("CreatedAtUnixTimestamp" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (RealIP x0) = object $
    [ "tag" .= ("RealIP" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (IP x0) = object $
    [ "tag" .= ("IP" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (WithBoard x0) = object $
    [ "tag" .= ("WithBoard" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (WithThread x0) = object $
    [ "tag" .= ("WithThread" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (WithThreadPosts x0) = object $
    [ "tag" .= ("WithThreadPosts" :: Text)
    , "contents" .= [toJSON x0]
    ]


instance Eq Param where
  (==) (Limit x0a) (Limit x0b) = x0a == x0b
  (==) (Offset x0a) (Offset x0b) = x0a == x0b
  (==) (SortOrder x0a) (SortOrder x0b) = x0a == x0b
  (==) (Order x0a) (Order x0b) = x0a == x0b
  (==) (ByUserId x0a) (ByUserId x0b) = x0a == x0b
  (==) (ByUsersIds x0a) (ByUsersIds x0b) = x0a == x0b
  (==) (ByUserName x0a) (ByUserName x0b) = x0a == x0b
  (==) (ByUsersNames x0a) (ByUsersNames x0b) = x0a == x0b
  (==) (ByForumId x0a) (ByForumId x0b) = x0a == x0b
  (==) (ByBoardId x0a) (ByBoardId x0b) = x0a == x0b
  (==) (ByBoardsIds x0a) (ByBoardsIds x0b) = x0a == x0b
  (==) (ByBoardName x0a) (ByBoardName x0b) = x0a == x0b
  (==) (ByThreadId x0a) (ByThreadId x0b) = x0a == x0b
  (==) (ByThreadsIds x0a) (ByThreadsIds x0b) = x0a == x0b
  (==) (ByThreadName x0a) (ByThreadName x0b) = x0a == x0b
  (==) (ByThreadPostId x0a) (ByThreadPostId x0b) = x0a == x0b
  (==) (ByThreadPostsIds x0a) (ByThreadPostsIds x0b) = x0a == x0b
  (==) (ByThreadPostName x0a) (ByThreadPostName x0b) = x0a == x0b
  (==) (ByThreadPostLikeId x0a) (ByThreadPostLikeId x0b) = x0a == x0b
  (==) (ByThreadPostLikesIds x0a) (ByThreadPostLikesIds x0b) = x0a == x0b
  (==) (ByPmId x0a) (ByPmId x0b) = x0a == x0b
  (==) (ByPmsIds x0a) (ByPmsIds x0b) = x0a == x0b
  (==) (ByReminderId x0a) (ByReminderId x0b) = x0a == x0b
  (==) (ByReminderFolderId x0a) (ByReminderFolderId x0b) = x0a == x0b
  (==) (ByParentId x0a) (ByParentId x0b) = x0a == x0b
  (==) (ByParentsIds x0a) (ByParentsIds x0b) = x0a == x0b
  (==) (ByParentName x0a) (ByParentName x0b) = x0a == x0b
  (==) (ByEmail x0a) (ByEmail x0b) = x0a == x0b
  (==) (BySelf x0a) (BySelf x0b) = x0a == x0b
  (==) (View x0a) (View x0b) = x0a == x0b
  (==) (Timestamp x0a) (Timestamp x0b) = x0a == x0b
  (==) (UnixTimestamp x0a) (UnixTimestamp x0b) = x0a == x0b
  (==) (CreatedAtTimestamp x0a) (CreatedAtTimestamp x0b) = x0a == x0b
  (==) (CreatedAtUnixTimestamp x0a) (CreatedAtUnixTimestamp x0b) = x0a == x0b
  (==) (RealIP x0a) (RealIP x0b) = x0a == x0b
  (==) (IP x0a) (IP x0b) = x0a == x0b
  (==) (WithBoard x0a) (WithBoard x0b) = x0a == x0b
  (==) (WithThread x0a) (WithThread x0b) = x0a == x0b
  (==) (WithThreadPosts x0a) (WithThreadPosts x0b) = x0a == x0b
  (==) _ _ = False

instance Show Param where
  show (Limit x0) = "limit: " <> show x0
  show (Offset x0) = "offset: " <> show x0
  show (SortOrder x0) = "sort_order: " <> show x0
  show (Order x0) = "order: " <> show x0
  show (ByUserId x0) = "by_user_id: " <> show x0
  show (ByUsersIds x0) = "by_users_ids: " <> show x0
  show (ByUserName x0) = "by_user_name: " <> show x0
  show (ByUsersNames x0) = "by_users_names: " <> show x0
  show (ByForumId x0) = "by_forum_id: " <> show x0
  show (ByBoardId x0) = "by_board_id: " <> show x0
  show (ByBoardsIds x0) = "by_boards_ids: " <> show x0
  show (ByBoardName x0) = "by_board_name: " <> show x0
  show (ByThreadId x0) = "by_thread_id: " <> show x0
  show (ByThreadsIds x0) = "by_threads_ids: " <> show x0
  show (ByThreadName x0) = "by_thread_name: " <> show x0
  show (ByThreadPostId x0) = "by_thread_post_id: " <> show x0
  show (ByThreadPostsIds x0) = "by_thread_posts_ids: " <> show x0
  show (ByThreadPostName x0) = "by_thread_post_name: " <> show x0
  show (ByThreadPostLikeId x0) = "by_thread_post_like_id: " <> show x0
  show (ByThreadPostLikesIds x0) = "by_thread_post_likes_ids: " <> show x0
  show (ByPmId x0) = "by_pm_id: " <> show x0
  show (ByPmsIds x0) = "by_pms_ids: " <> show x0
  show (ByReminderId x0) = "by_reminder_id: " <> show x0
  show (ByReminderFolderId x0) = "by_reminder_folder_id: " <> show x0
  show (ByParentId x0) = "by_parent_id: " <> show x0
  show (ByParentsIds x0) = "by_parents_ids: " <> show x0
  show (ByParentName x0) = "by_parent_name: " <> show x0
  show (ByEmail x0) = "by_email: " <> show x0
  show (BySelf x0) = "by_self: " <> show x0
  show (View x0) = "view: " <> show x0
  show (Timestamp x0) = "timestamp: " <> show x0
  show (UnixTimestamp x0) = "unix_timestamp: " <> show x0
  show (CreatedAtTimestamp x0) = "created_at_timestamp: " <> show x0
  show (CreatedAtUnixTimestamp x0) = "created_at_unix_timestamp: " <> show x0
  show (RealIP x0) = "real_ip: " <> show x0
  show (IP x0) = "ip: " <> show x0
  show (WithBoard x0) = "with_board: " <> show x0
  show (WithThread x0) = "with_thread: " <> show x0
  show (WithThreadPosts x0) = "with_thread_posts: " <> show x0


instance QueryParam Param where
  qp (Limit x0) = ("limit", (T.pack $ show x0))
  qp (Offset x0) = ("offset", (T.pack $ show x0))
  qp (SortOrder x0) = ("sort_order", (T.pack $ show x0))
  qp (Order x0) = ("order", (T.pack $ show x0))
  qp (ByUserId x0) = ("by_user_id", (T.pack $ show x0))
  qp (ByUsersIds x0) = ("by_users_ids", (T.pack $ show x0))
  qp (ByUserName x0) = ("by_user_name", x0)
  qp (ByUsersNames x0) = ("by_users_names", (T.pack $ show x0))
  qp (ByForumId x0) = ("by_forum_id", (T.pack $ show x0))
  qp (ByBoardId x0) = ("by_board_id", (T.pack $ show x0))
  qp (ByBoardsIds x0) = ("by_boards_ids", (T.pack $ show x0))
  qp (ByBoardName x0) = ("by_board_name", x0)
  qp (ByThreadId x0) = ("by_thread_id", (T.pack $ show x0))
  qp (ByThreadsIds x0) = ("by_threads_ids", (T.pack $ show x0))
  qp (ByThreadName x0) = ("by_thread_name", x0)
  qp (ByThreadPostId x0) = ("by_thread_post_id", (T.pack $ show x0))
  qp (ByThreadPostsIds x0) = ("by_thread_posts_ids", (T.pack $ show x0))
  qp (ByThreadPostName x0) = ("by_thread_post_name", x0)
  qp (ByThreadPostLikeId x0) = ("by_thread_post_like_id", (T.pack $ show x0))
  qp (ByThreadPostLikesIds x0) = ("by_thread_post_likes_ids", (T.pack $ show x0))
  qp (ByPmId x0) = ("by_pm_id", (T.pack $ show x0))
  qp (ByPmsIds x0) = ("by_pms_ids", (T.pack $ show x0))
  qp (ByReminderId x0) = ("by_reminder_id", (T.pack $ show x0))
  qp (ByReminderFolderId x0) = ("by_reminder_folder_id", (T.pack $ show x0))
  qp (ByParentId x0) = ("by_parent_id", (T.pack $ show x0))
  qp (ByParentsIds x0) = ("by_parents_ids", (T.pack $ show x0))
  qp (ByParentName x0) = ("by_parent_name", x0)
  qp (ByEmail x0) = ("by_email", x0)
  qp (BySelf x0) = ("by_self", (T.pack $ show x0))
  qp (View x0) = ("view", (T.pack $ show x0))
  qp (Timestamp x0) = ("timestamp", (T.pack $ show x0))
  qp (UnixTimestamp x0) = ("unix_timestamp", (T.pack $ show x0))
  qp (CreatedAtTimestamp x0) = ("created_at_timestamp", (T.pack $ show x0))
  qp (CreatedAtUnixTimestamp x0) = ("created_at_unix_timestamp", (T.pack $ show x0))
  qp (RealIP x0) = ("real_ip", x0)
  qp (IP x0) = ("ip", x0)
  qp (WithBoard x0) = ("with_board", (T.pack $ show x0))
  qp (WithThread x0) = ("with_thread", (T.pack $ show x0))
  qp (WithThreadPosts x0) = ("with_thread_posts", (T.pack $ show x0))


data ParamTag
  = ParamTag_Limit 
  | ParamTag_Offset 
  | ParamTag_SortOrder 
  | ParamTag_Order 
  | ParamTag_ByUserId 
  | ParamTag_ByUsersIds 
  | ParamTag_ByUserName 
  | ParamTag_ByUsersNames 
  | ParamTag_ByBoardId 
  | ParamTag_ByBoardsIds 
  | ParamTag_ByBoardName 
  | ParamTag_ByThreadId 
  | ParamTag_ByThreadsIds 
  | ParamTag_ByThreadName 
  | ParamTag_ByThreadPostId 
  | ParamTag_ByThreadPostsIds 
  | ParamTag_ByThreadPostName 
  | ParamTag_ByThreadPostLikeId 
  | ParamTag_ByThreadPostLikesIds 
  | ParamTag_ByPmId 
  | ParamTag_ByPmsIds 
  | ParamTag_ByReminderId 
  | ParamTag_ByReminderFolderId 
  | ParamTag_ByParentId 
  | ParamTag_ByParentsIds 
  | ParamTag_ByParentName 
  | ParamTag_ByEmail 
  | ParamTag_BySelf 
  | ParamTag_View 
  | ParamTag_Timestamp 
  | ParamTag_UnixTimestamp 
  | ParamTag_CreatedAtTimestamp 
  | ParamTag_CreatedAtUnixTimestamp 
  | ParamTag_RealIP 
  | ParamTag_IP 
  | ParamTag_WithBoard 
  | ParamTag_WithThread 
  | ParamTag_WithThreadPosts 
  deriving (Generic,Typeable,NFData,Ord)


instance FromJSON ParamTag where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("ParamTag_Limit" :: Text) -> do
        pure ParamTag_Limit

      ("ParamTag_Offset" :: Text) -> do
        pure ParamTag_Offset

      ("ParamTag_SortOrder" :: Text) -> do
        pure ParamTag_SortOrder

      ("ParamTag_Order" :: Text) -> do
        pure ParamTag_Order

      ("ParamTag_ByUserId" :: Text) -> do
        pure ParamTag_ByUserId

      ("ParamTag_ByUsersIds" :: Text) -> do
        pure ParamTag_ByUsersIds

      ("ParamTag_ByUserName" :: Text) -> do
        pure ParamTag_ByUserName

      ("ParamTag_ByUsersNames" :: Text) -> do
        pure ParamTag_ByUsersNames

      ("ParamTag_ByBoardId" :: Text) -> do
        pure ParamTag_ByBoardId

      ("ParamTag_ByBoardsIds" :: Text) -> do
        pure ParamTag_ByBoardsIds

      ("ParamTag_ByBoardName" :: Text) -> do
        pure ParamTag_ByBoardName

      ("ParamTag_ByThreadId" :: Text) -> do
        pure ParamTag_ByThreadId

      ("ParamTag_ByThreadsIds" :: Text) -> do
        pure ParamTag_ByThreadsIds

      ("ParamTag_ByThreadName" :: Text) -> do
        pure ParamTag_ByThreadName

      ("ParamTag_ByThreadPostId" :: Text) -> do
        pure ParamTag_ByThreadPostId

      ("ParamTag_ByThreadPostsIds" :: Text) -> do
        pure ParamTag_ByThreadPostsIds

      ("ParamTag_ByThreadPostName" :: Text) -> do
        pure ParamTag_ByThreadPostName

      ("ParamTag_ByThreadPostLikeId" :: Text) -> do
        pure ParamTag_ByThreadPostLikeId

      ("ParamTag_ByThreadPostLikesIds" :: Text) -> do
        pure ParamTag_ByThreadPostLikesIds

      ("ParamTag_ByPmId" :: Text) -> do
        pure ParamTag_ByPmId

      ("ParamTag_ByPmsIds" :: Text) -> do
        pure ParamTag_ByPmsIds

      ("ParamTag_ByReminderId" :: Text) -> do
        pure ParamTag_ByReminderId

      ("ParamTag_ByReminderFolderId" :: Text) -> do
        pure ParamTag_ByReminderFolderId

      ("ParamTag_ByParentId" :: Text) -> do
        pure ParamTag_ByParentId

      ("ParamTag_ByParentsIds" :: Text) -> do
        pure ParamTag_ByParentsIds

      ("ParamTag_ByParentName" :: Text) -> do
        pure ParamTag_ByParentName

      ("ParamTag_ByEmail" :: Text) -> do
        pure ParamTag_ByEmail

      ("ParamTag_BySelf" :: Text) -> do
        pure ParamTag_BySelf

      ("ParamTag_View" :: Text) -> do
        pure ParamTag_View

      ("ParamTag_Timestamp" :: Text) -> do
        pure ParamTag_Timestamp

      ("ParamTag_UnixTimestamp" :: Text) -> do
        pure ParamTag_UnixTimestamp

      ("ParamTag_CreatedAtTimestamp" :: Text) -> do
        pure ParamTag_CreatedAtTimestamp

      ("ParamTag_CreatedAtUnixTimestamp" :: Text) -> do
        pure ParamTag_CreatedAtUnixTimestamp

      ("ParamTag_RealIP" :: Text) -> do
        pure ParamTag_RealIP

      ("ParamTag_IP" :: Text) -> do
        pure ParamTag_IP

      ("ParamTag_WithBoard" :: Text) -> do
        pure ParamTag_WithBoard

      ("ParamTag_WithThread" :: Text) -> do
        pure ParamTag_WithThread

      ("ParamTag_WithThreadPosts" :: Text) -> do
        pure ParamTag_WithThreadPosts

      _ -> fail "Could not parse ParamTag"

  parseJSON x = fail $ "ParamTag: Could not parse object: " <> show x


instance ToJSON ParamTag where
  toJSON (ParamTag_Limit ) = object $
    [ "tag" .= ("ParamTag_Limit" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_Offset ) = object $
    [ "tag" .= ("ParamTag_Offset" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_SortOrder ) = object $
    [ "tag" .= ("ParamTag_SortOrder" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_Order ) = object $
    [ "tag" .= ("ParamTag_Order" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUserId ) = object $
    [ "tag" .= ("ParamTag_ByUserId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUsersIds ) = object $
    [ "tag" .= ("ParamTag_ByUsersIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUserName ) = object $
    [ "tag" .= ("ParamTag_ByUserName" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByUsersNames ) = object $
    [ "tag" .= ("ParamTag_ByUsersNames" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByBoardId ) = object $
    [ "tag" .= ("ParamTag_ByBoardId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByBoardsIds ) = object $
    [ "tag" .= ("ParamTag_ByBoardsIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByBoardName ) = object $
    [ "tag" .= ("ParamTag_ByBoardName" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadId ) = object $
    [ "tag" .= ("ParamTag_ByThreadId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadsIds ) = object $
    [ "tag" .= ("ParamTag_ByThreadsIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadName ) = object $
    [ "tag" .= ("ParamTag_ByThreadName" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadPostId ) = object $
    [ "tag" .= ("ParamTag_ByThreadPostId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadPostsIds ) = object $
    [ "tag" .= ("ParamTag_ByThreadPostsIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadPostName ) = object $
    [ "tag" .= ("ParamTag_ByThreadPostName" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadPostLikeId ) = object $
    [ "tag" .= ("ParamTag_ByThreadPostLikeId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByThreadPostLikesIds ) = object $
    [ "tag" .= ("ParamTag_ByThreadPostLikesIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByPmId ) = object $
    [ "tag" .= ("ParamTag_ByPmId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByPmsIds ) = object $
    [ "tag" .= ("ParamTag_ByPmsIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByReminderId ) = object $
    [ "tag" .= ("ParamTag_ByReminderId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByReminderFolderId ) = object $
    [ "tag" .= ("ParamTag_ByReminderFolderId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByParentId ) = object $
    [ "tag" .= ("ParamTag_ByParentId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByParentsIds ) = object $
    [ "tag" .= ("ParamTag_ByParentsIds" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByParentName ) = object $
    [ "tag" .= ("ParamTag_ByParentName" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_ByEmail ) = object $
    [ "tag" .= ("ParamTag_ByEmail" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_BySelf ) = object $
    [ "tag" .= ("ParamTag_BySelf" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_View ) = object $
    [ "tag" .= ("ParamTag_View" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_Timestamp ) = object $
    [ "tag" .= ("ParamTag_Timestamp" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_UnixTimestamp ) = object $
    [ "tag" .= ("ParamTag_UnixTimestamp" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_CreatedAtTimestamp ) = object $
    [ "tag" .= ("ParamTag_CreatedAtTimestamp" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_CreatedAtUnixTimestamp ) = object $
    [ "tag" .= ("ParamTag_CreatedAtUnixTimestamp" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_RealIP ) = object $
    [ "tag" .= ("ParamTag_RealIP" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_IP ) = object $
    [ "tag" .= ("ParamTag_IP" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_WithBoard ) = object $
    [ "tag" .= ("ParamTag_WithBoard" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_WithThread ) = object $
    [ "tag" .= ("ParamTag_WithThread" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (ParamTag_WithThreadPosts ) = object $
    [ "tag" .= ("ParamTag_WithThreadPosts" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq ParamTag where
  (==) ParamTag_Limit ParamTag_Limit = True
  (==) ParamTag_Offset ParamTag_Offset = True
  (==) ParamTag_SortOrder ParamTag_SortOrder = True
  (==) ParamTag_Order ParamTag_Order = True
  (==) ParamTag_ByUserId ParamTag_ByUserId = True
  (==) ParamTag_ByUsersIds ParamTag_ByUsersIds = True
  (==) ParamTag_ByUserName ParamTag_ByUserName = True
  (==) ParamTag_ByUsersNames ParamTag_ByUsersNames = True
  (==) ParamTag_ByBoardId ParamTag_ByBoardId = True
  (==) ParamTag_ByBoardsIds ParamTag_ByBoardsIds = True
  (==) ParamTag_ByBoardName ParamTag_ByBoardName = True
  (==) ParamTag_ByThreadId ParamTag_ByThreadId = True
  (==) ParamTag_ByThreadsIds ParamTag_ByThreadsIds = True
  (==) ParamTag_ByThreadName ParamTag_ByThreadName = True
  (==) ParamTag_ByThreadPostId ParamTag_ByThreadPostId = True
  (==) ParamTag_ByThreadPostsIds ParamTag_ByThreadPostsIds = True
  (==) ParamTag_ByThreadPostName ParamTag_ByThreadPostName = True
  (==) ParamTag_ByThreadPostLikeId ParamTag_ByThreadPostLikeId = True
  (==) ParamTag_ByThreadPostLikesIds ParamTag_ByThreadPostLikesIds = True
  (==) ParamTag_ByPmId ParamTag_ByPmId = True
  (==) ParamTag_ByPmsIds ParamTag_ByPmsIds = True
  (==) ParamTag_ByReminderId ParamTag_ByReminderId = True
  (==) ParamTag_ByReminderFolderId ParamTag_ByReminderFolderId = True
  (==) ParamTag_ByParentId ParamTag_ByParentId = True
  (==) ParamTag_ByParentsIds ParamTag_ByParentsIds = True
  (==) ParamTag_ByParentName ParamTag_ByParentName = True
  (==) ParamTag_ByEmail ParamTag_ByEmail = True
  (==) ParamTag_BySelf ParamTag_BySelf = True
  (==) ParamTag_View ParamTag_View = True
  (==) ParamTag_Timestamp ParamTag_Timestamp = True
  (==) ParamTag_UnixTimestamp ParamTag_UnixTimestamp = True
  (==) ParamTag_CreatedAtTimestamp ParamTag_CreatedAtTimestamp = True
  (==) ParamTag_CreatedAtUnixTimestamp ParamTag_CreatedAtUnixTimestamp = True
  (==) ParamTag_RealIP ParamTag_RealIP = True
  (==) ParamTag_IP ParamTag_IP = True
  (==) ParamTag_WithBoard ParamTag_WithBoard = True
  (==) ParamTag_WithThread ParamTag_WithThread = True
  (==) ParamTag_WithThreadPosts ParamTag_WithThreadPosts = True
  (==) _ _ = False

instance Show ParamTag where
  show ParamTag_Limit = "limit"
  show ParamTag_Offset = "offset"
  show ParamTag_SortOrder = "sort_order"
  show ParamTag_Order = "order"
  show ParamTag_ByUserId = "by_user_id"
  show ParamTag_ByUsersIds = "by_users_ids"
  show ParamTag_ByUserName = "by_user_name"
  show ParamTag_ByUsersNames = "by_users_names"
  show ParamTag_ByBoardId = "by_board_id"
  show ParamTag_ByBoardsIds = "by_boards_ids"
  show ParamTag_ByBoardName = "by_board_name"
  show ParamTag_ByThreadId = "by_thread_id"
  show ParamTag_ByThreadsIds = "by_threads_ids"
  show ParamTag_ByThreadName = "by_thread_name"
  show ParamTag_ByThreadPostId = "by_thread_post_id"
  show ParamTag_ByThreadPostsIds = "by_thread_posts_ids"
  show ParamTag_ByThreadPostName = "by_thread_post_name"
  show ParamTag_ByThreadPostLikeId = "by_thread_post_like_id"
  show ParamTag_ByThreadPostLikesIds = "by_thread_post_likes_ids"
  show ParamTag_ByPmId = "by_pm_id"
  show ParamTag_ByPmsIds = "by_pms_ids"
  show ParamTag_ByReminderId = "by_reminder_id"
  show ParamTag_ByReminderFolderId = "by_reminder_folder_id"
  show ParamTag_ByParentId = "by_parent_id"
  show ParamTag_ByParentsIds = "by_parents_ids"
  show ParamTag_ByParentName = "by_parent_name"
  show ParamTag_ByEmail = "by_email"
  show ParamTag_BySelf = "by_self"
  show ParamTag_View = "view"
  show ParamTag_Timestamp = "timestamp"
  show ParamTag_UnixTimestamp = "unix_timestamp"
  show ParamTag_CreatedAtTimestamp = "created_at_timestamp"
  show ParamTag_CreatedAtUnixTimestamp = "created_at_unix_timestamp"
  show ParamTag_RealIP = "real_ip"
  show ParamTag_IP = "ip"
  show ParamTag_WithBoard = "with_board"
  show ParamTag_WithThread = "with_thread"
  show ParamTag_WithThreadPosts = "with_thread_posts"


instance Read ParamTag where
  readsPrec _ "limit" = [(ParamTag_Limit, "")]
  readsPrec _ "offset" = [(ParamTag_Offset, "")]
  readsPrec _ "sort_order" = [(ParamTag_SortOrder, "")]
  readsPrec _ "order" = [(ParamTag_Order, "")]
  readsPrec _ "by_user_id" = [(ParamTag_ByUserId, "")]
  readsPrec _ "by_users_ids" = [(ParamTag_ByUsersIds, "")]
  readsPrec _ "by_user_name" = [(ParamTag_ByUserName, "")]
  readsPrec _ "by_users_names" = [(ParamTag_ByUsersNames, "")]
  readsPrec _ "by_board_id" = [(ParamTag_ByBoardId, "")]
  readsPrec _ "by_boards_ids" = [(ParamTag_ByBoardsIds, "")]
  readsPrec _ "by_board_name" = [(ParamTag_ByBoardName, "")]
  readsPrec _ "by_thread_id" = [(ParamTag_ByThreadId, "")]
  readsPrec _ "by_threads_ids" = [(ParamTag_ByThreadsIds, "")]
  readsPrec _ "by_thread_name" = [(ParamTag_ByThreadName, "")]
  readsPrec _ "by_thread_post_id" = [(ParamTag_ByThreadPostId, "")]
  readsPrec _ "by_thread_posts_ids" = [(ParamTag_ByThreadPostsIds, "")]
  readsPrec _ "by_thread_post_name" = [(ParamTag_ByThreadPostName, "")]
  readsPrec _ "by_thread_post_like_id" = [(ParamTag_ByThreadPostLikeId, "")]
  readsPrec _ "by_thread_post_likes_ids" = [(ParamTag_ByThreadPostLikesIds, "")]
  readsPrec _ "by_pm_id" = [(ParamTag_ByPmId, "")]
  readsPrec _ "by_pms_ids" = [(ParamTag_ByPmsIds, "")]
  readsPrec _ "by_reminder_id" = [(ParamTag_ByReminderId, "")]
  readsPrec _ "by_reminder_folder_id" = [(ParamTag_ByReminderFolderId, "")]
  readsPrec _ "by_parent_id" = [(ParamTag_ByParentId, "")]
  readsPrec _ "by_parents_ids" = [(ParamTag_ByParentsIds, "")]
  readsPrec _ "by_parent_name" = [(ParamTag_ByParentName, "")]
  readsPrec _ "by_email" = [(ParamTag_ByEmail, "")]
  readsPrec _ "by_self" = [(ParamTag_BySelf, "")]
  readsPrec _ "view" = [(ParamTag_View, "")]
  readsPrec _ "timestamp" = [(ParamTag_Timestamp, "")]
  readsPrec _ "unix_timestamp" = [(ParamTag_UnixTimestamp, "")]
  readsPrec _ "created_at_timestamp" = [(ParamTag_CreatedAtTimestamp, "")]
  readsPrec _ "created_at_unix_timestamp" = [(ParamTag_CreatedAtUnixTimestamp, "")]
  readsPrec _ "real_ip" = [(ParamTag_RealIP, "")]
  readsPrec _ "ip" = [(ParamTag_IP, "")]
  readsPrec _ "with_board" = [(ParamTag_WithBoard, "")]
  readsPrec _ "with_thread" = [(ParamTag_WithThread, "")]
  readsPrec _ "with_thread_posts" = [(ParamTag_WithThreadPosts, "")]
  readsPrec _ _ = []


data SortOrderBy
  = SortOrderBy_Asc 
  | SortOrderBy_Dsc 
  | SortOrderBy_Rnd 
  | SortOrderBy_None 
  deriving (Generic,Typeable,NFData)


instance FromJSON SortOrderBy where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("SortOrderBy_Asc" :: Text) -> do
        pure SortOrderBy_Asc

      ("SortOrderBy_Dsc" :: Text) -> do
        pure SortOrderBy_Dsc

      ("SortOrderBy_Rnd" :: Text) -> do
        pure SortOrderBy_Rnd

      ("SortOrderBy_None" :: Text) -> do
        pure SortOrderBy_None

      _ -> fail "Could not parse SortOrderBy"

  parseJSON x = fail $ "SortOrderBy: Could not parse object: " <> show x


instance ToJSON SortOrderBy where
  toJSON (SortOrderBy_Asc ) = object $
    [ "tag" .= ("SortOrderBy_Asc" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (SortOrderBy_Dsc ) = object $
    [ "tag" .= ("SortOrderBy_Dsc" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (SortOrderBy_Rnd ) = object $
    [ "tag" .= ("SortOrderBy_Rnd" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (SortOrderBy_None ) = object $
    [ "tag" .= ("SortOrderBy_None" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq SortOrderBy where
  (==) SortOrderBy_Asc SortOrderBy_Asc = True
  (==) SortOrderBy_Dsc SortOrderBy_Dsc = True
  (==) SortOrderBy_Rnd SortOrderBy_Rnd = True
  (==) SortOrderBy_None SortOrderBy_None = True
  (==) _ _ = False

instance Show SortOrderBy where
  show SortOrderBy_Asc = "asc"
  show SortOrderBy_Dsc = "dsc"
  show SortOrderBy_Rnd = "rnd"
  show SortOrderBy_None = "none"


instance Read SortOrderBy where
  readsPrec _ "asc" = [(SortOrderBy_Asc, "")]
  readsPrec _ "dsc" = [(SortOrderBy_Dsc, "")]
  readsPrec _ "rnd" = [(SortOrderBy_Rnd, "")]
  readsPrec _ "none" = [(SortOrderBy_None, "")]
  readsPrec _ _ = []


data OrderBy
  = OrderBy_UserId 
  | OrderBy_CreatedAt 
  | OrderBy_ModifiedAt 
  | OrderBy_ModifiedBy 
  | OrderBy_ActivityAt 
  | OrderBy_BoardId 
  | OrderBy_ThreadId 
  | OrderBy_Id 
  | OrderBy_None 
  deriving (Generic,Typeable,NFData)


instance FromJSON OrderBy where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Text)
    case tag of
      ("OrderBy_UserId" :: Text) -> do
        pure OrderBy_UserId

      ("OrderBy_CreatedAt" :: Text) -> do
        pure OrderBy_CreatedAt

      ("OrderBy_ModifiedAt" :: Text) -> do
        pure OrderBy_ModifiedAt

      ("OrderBy_ModifiedBy" :: Text) -> do
        pure OrderBy_ModifiedBy

      ("OrderBy_ActivityAt" :: Text) -> do
        pure OrderBy_ActivityAt

      ("OrderBy_BoardId" :: Text) -> do
        pure OrderBy_BoardId

      ("OrderBy_ThreadId" :: Text) -> do
        pure OrderBy_ThreadId

      ("OrderBy_Id" :: Text) -> do
        pure OrderBy_Id

      ("OrderBy_None" :: Text) -> do
        pure OrderBy_None

      _ -> fail "Could not parse OrderBy"

  parseJSON x = fail $ "OrderBy: Could not parse object: " <> show x


instance ToJSON OrderBy where
  toJSON (OrderBy_UserId ) = object $
    [ "tag" .= ("OrderBy_UserId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_CreatedAt ) = object $
    [ "tag" .= ("OrderBy_CreatedAt" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_ModifiedAt ) = object $
    [ "tag" .= ("OrderBy_ModifiedAt" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_ModifiedBy ) = object $
    [ "tag" .= ("OrderBy_ModifiedBy" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_ActivityAt ) = object $
    [ "tag" .= ("OrderBy_ActivityAt" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_BoardId ) = object $
    [ "tag" .= ("OrderBy_BoardId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_ThreadId ) = object $
    [ "tag" .= ("OrderBy_ThreadId" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_Id ) = object $
    [ "tag" .= ("OrderBy_Id" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (OrderBy_None ) = object $
    [ "tag" .= ("OrderBy_None" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq OrderBy where
  (==) OrderBy_UserId OrderBy_UserId = True
  (==) OrderBy_CreatedAt OrderBy_CreatedAt = True
  (==) OrderBy_ModifiedAt OrderBy_ModifiedAt = True
  (==) OrderBy_ModifiedBy OrderBy_ModifiedBy = True
  (==) OrderBy_ActivityAt OrderBy_ActivityAt = True
  (==) OrderBy_BoardId OrderBy_BoardId = True
  (==) OrderBy_ThreadId OrderBy_ThreadId = True
  (==) OrderBy_Id OrderBy_Id = True
  (==) OrderBy_None OrderBy_None = True
  (==) _ _ = False

instance Show OrderBy where
  show OrderBy_UserId = "user_id"
  show OrderBy_CreatedAt = "created_at"
  show OrderBy_ModifiedAt = "modified_at"
  show OrderBy_ModifiedBy = "modified_by"
  show OrderBy_ActivityAt = "activity_at"
  show OrderBy_BoardId = "board_id"
  show OrderBy_ThreadId = "thread_id"
  show OrderBy_Id = "id"
  show OrderBy_None = "none"


instance Read OrderBy where
  readsPrec _ "user_id" = [(OrderBy_UserId, "")]
  readsPrec _ "created_at" = [(OrderBy_CreatedAt, "")]
  readsPrec _ "modified_at" = [(OrderBy_ModifiedAt, "")]
  readsPrec _ "modified_by" = [(OrderBy_ModifiedBy, "")]
  readsPrec _ "activity_at" = [(OrderBy_ActivityAt, "")]
  readsPrec _ "board_id" = [(OrderBy_BoardId, "")]
  readsPrec _ "thread_id" = [(OrderBy_ThreadId, "")]
  readsPrec _ "id" = [(OrderBy_Id, "")]
  readsPrec _ "none" = [(OrderBy_None, "")]
  readsPrec _ _ = []

-- footer