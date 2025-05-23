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

data PostData
  = PostDataRaw !(Text)
  | PostDataMarkdown !(Text)
  | PostDataBBCode !(Text)
  | PostDataCode !(Text) !(Text)
  | PostDataOther !(Text) !(Text)
  | PostDataEmpty 
  deriving (Generic,Typeable,NFData)


instance FromJSON PostData where
  parseJSON (Object o) = do
    tag <- o .: ("tag")
    case tag of
      ("PostDataRaw" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> PostDataRaw <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: PostDataRaw"

      ("PostDataMarkdown" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> PostDataMarkdown <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: PostDataMarkdown"

      ("PostDataBBCode" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> PostDataBBCode <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: PostDataBBCode"

      ("PostDataCode" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> PostDataCode <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: PostDataCode"

      ("PostDataOther" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> PostDataOther <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: PostDataOther"

      ("PostDataEmpty" :: Text) -> do
        pure PostDataEmpty

      _ -> fail "Could not parse PostData"

  parseJSON x = fail $ "PostData: Could not parse object: " <> show x


instance ToJSON PostData where
  toJSON (PostDataRaw x0) = object $
    [ "tag" .= ("PostDataRaw" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (PostDataMarkdown x0) = object $
    [ "tag" .= ("PostDataMarkdown" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (PostDataBBCode x0) = object $
    [ "tag" .= ("PostDataBBCode" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (PostDataCode x0 x1) = object $
    [ "tag" .= ("PostDataCode" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (PostDataOther x0 x1) = object $
    [ "tag" .= ("PostDataOther" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (PostDataEmpty ) = object $
    [ "tag" .= ("PostDataEmpty" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq PostData where
  (==) (PostDataRaw x0a) (PostDataRaw x0b) = x0a == x0b
  (==) (PostDataMarkdown x0a) (PostDataMarkdown x0b) = x0a == x0b
  (==) (PostDataBBCode x0a) (PostDataBBCode x0b) = x0a == x0b
  (==) (PostDataCode x0a x1a) (PostDataCode x0b x1b) = x0a == x0b && x1a == x1b
  (==) (PostDataOther x0a x1a) (PostDataOther x0b x1b) = x0a == x0b && x1a == x1b
  (==) PostDataEmpty PostDataEmpty = True
  (==) _ _ = False

instance Show PostData where
  show (PostDataRaw x0) = "post_data_raw: " <> show x0
  show (PostDataMarkdown x0) = "post_data_markdown: " <> show x0
  show (PostDataBBCode x0) = "post_data_bbcode: " <> show x0
  show (PostDataCode x0 x1) = "post_data_code: " <> show x0 <> " " <> show x1
  show (PostDataOther x0 x1) = "post_data_other: " <> show x0 <> " " <> show x1
  show PostDataEmpty = "empty"


data TyPostData
  = TyPostDataRaw 
  | TyPostDataMarkdown 
  | TyPostDataBBCode 
  | TyPostDataCode 
  | TyPostDataOther 
  | TyPostDataEmpty 
  deriving (Generic,Typeable,NFData)


instance FromJSON TyPostData where
  parseJSON (Object o) = do
    tag <- o .: ("tag")
    case tag of
      ("TyPostDataRaw" :: Text) -> do
        pure TyPostDataRaw

      ("TyPostDataMarkdown" :: Text) -> do
        pure TyPostDataMarkdown

      ("TyPostDataBBCode" :: Text) -> do
        pure TyPostDataBBCode

      ("TyPostDataCode" :: Text) -> do
        pure TyPostDataCode

      ("TyPostDataOther" :: Text) -> do
        pure TyPostDataOther

      ("TyPostDataEmpty" :: Text) -> do
        pure TyPostDataEmpty

      _ -> fail "Could not parse TyPostData"

  parseJSON x = fail $ "TyPostData: Could not parse object: " <> show x


instance ToJSON TyPostData where
  toJSON (TyPostDataRaw ) = object $
    [ "tag" .= ("TyPostDataRaw" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyPostDataMarkdown ) = object $
    [ "tag" .= ("TyPostDataMarkdown" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyPostDataBBCode ) = object $
    [ "tag" .= ("TyPostDataBBCode" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyPostDataCode ) = object $
    [ "tag" .= ("TyPostDataCode" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyPostDataOther ) = object $
    [ "tag" .= ("TyPostDataOther" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (TyPostDataEmpty ) = object $
    [ "tag" .= ("TyPostDataEmpty" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TyPostData where
  (==) TyPostDataRaw TyPostDataRaw = True
  (==) TyPostDataMarkdown TyPostDataMarkdown = True
  (==) TyPostDataBBCode TyPostDataBBCode = True
  (==) TyPostDataCode TyPostDataCode = True
  (==) TyPostDataOther TyPostDataOther = True
  (==) TyPostDataEmpty TyPostDataEmpty = True
  (==) _ _ = False

instance Show TyPostData where
  show TyPostDataRaw = "raw"
  show TyPostDataMarkdown = "markdown"
  show TyPostDataBBCode = "bbcode"
  show TyPostDataCode = "code"
  show TyPostDataOther = "other"
  show TyPostDataEmpty = "empty"


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
    threadPostRequestTitle <- o .: ("title")
    threadPostRequestBody <- o .: ("body")
    threadPostRequestTags <- o .: ("tags")
    threadPostRequestPrivateTags <- o .: ("private_tags")
    threadPostRequestGuard <- o .: ("guard")
    threadPostRequestStateTag <- o .: ("state_tag")
    threadPostRequestStatePrivateTag <- o .: ("state_private_tag")
    pure $ ThreadPostRequest {
      threadPostRequestTitle = threadPostRequestTitle,
      threadPostRequestBody = threadPostRequestBody,
      threadPostRequestTags = threadPostRequestTags,
      threadPostRequestPrivateTags = threadPostRequestPrivateTags,
      threadPostRequestGuard = threadPostRequestGuard,
      threadPostRequestStateTag = threadPostRequestStateTag,
      threadPostRequestStatePrivateTag = threadPostRequestStatePrivateTag
    }
  parseJSON x = fail $ "ThreadPostRequest: Could not parse object: " <> show x


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
    threadPostResponseId <- o .: ("id")
    threadPostResponseUserId <- o .: ("user_id")
    threadPostResponseBoardId <- o .: ("board_id")
    threadPostResponseThreadId <- o .: ("thread_id")
    threadPostResponseParentId <- o .: ("parent_id")
    threadPostResponseTitle <- o .: ("title")
    threadPostResponseBody <- o .: ("body")
    threadPostResponseTags <- o .: ("tags")
    threadPostResponsePrivateTags <- o .: ("private_tags")
    threadPostResponseActive <- o .: ("active")
    threadPostResponseGuard <- o .: ("guard")
    threadPostResponseCreatedAt <- o .: ("created_at")
    threadPostResponseModifiedBy <- o .: ("modified_by")
    threadPostResponseModifiedAt <- o .: ("modified_at")
    threadPostResponseActivityAt <- o .: ("activity_at")
    pure $ ThreadPostResponse {
      threadPostResponseId = threadPostResponseId,
      threadPostResponseUserId = threadPostResponseUserId,
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
  parseJSON x = fail $ "ThreadPostResponse: Could not parse object: " <> show x


instance ToJSON ThreadPostResponse where
  toJSON ThreadPostResponse{..} = object $
    [ "tag" .= ("ThreadPostResponse" :: Text)
    , "id" .= threadPostResponseId
    , "user_id" .= threadPostResponseUserId
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
  (==) a b = threadPostResponseId a == threadPostResponseId b && threadPostResponseUserId a == threadPostResponseUserId b && threadPostResponseBoardId a == threadPostResponseBoardId b && threadPostResponseThreadId a == threadPostResponseThreadId b && threadPostResponseParentId a == threadPostResponseParentId b && threadPostResponseTitle a == threadPostResponseTitle b && threadPostResponseBody a == threadPostResponseBody b && threadPostResponseTags a == threadPostResponseTags b && threadPostResponsePrivateTags a == threadPostResponsePrivateTags b && threadPostResponseActive a == threadPostResponseActive b && threadPostResponseGuard a == threadPostResponseGuard b && threadPostResponseCreatedAt a == threadPostResponseCreatedAt b && threadPostResponseModifiedBy a == threadPostResponseModifiedBy b && threadPostResponseModifiedAt a == threadPostResponseModifiedAt b && threadPostResponseActivityAt a == threadPostResponseActivityAt b

instance Show ThreadPostResponse where
    show rec = "threadPostResponseId: " <> show (threadPostResponseId rec) <> ", " <> "threadPostResponseUserId: " <> show (threadPostResponseUserId rec) <> ", " <> "threadPostResponseBoardId: " <> show (threadPostResponseBoardId rec) <> ", " <> "threadPostResponseThreadId: " <> show (threadPostResponseThreadId rec) <> ", " <> "threadPostResponseParentId: " <> show (threadPostResponseParentId rec) <> ", " <> "threadPostResponseTitle: " <> show (threadPostResponseTitle rec) <> ", " <> "threadPostResponseBody: " <> show (threadPostResponseBody rec) <> ", " <> "threadPostResponseTags: " <> show (threadPostResponseTags rec) <> ", " <> "threadPostResponsePrivateTags: " <> show (threadPostResponsePrivateTags rec) <> ", " <> "threadPostResponseActive: " <> show (threadPostResponseActive rec) <> ", " <> "threadPostResponseGuard: " <> show (threadPostResponseGuard rec) <> ", " <> "threadPostResponseCreatedAt: " <> show (threadPostResponseCreatedAt rec) <> ", " <> "threadPostResponseModifiedBy: " <> show (threadPostResponseModifiedBy rec) <> ", " <> "threadPostResponseModifiedAt: " <> show (threadPostResponseModifiedAt rec) <> ", " <> "threadPostResponseActivityAt: " <> show (threadPostResponseActivityAt rec)

data ThreadPostResponses = ThreadPostResponses {
  threadPostResponses :: !([ThreadPostResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostResponses where
  parseJSON (Object o) = do
    threadPostResponses <- o .: ("thread_post_responses")
    pure $ ThreadPostResponses {
      threadPostResponses = threadPostResponses
    }
  parseJSON x = fail $ "ThreadPostResponses: Could not parse object: " <> show x


instance ToJSON ThreadPostResponses where
  toJSON ThreadPostResponses{..} = object $
    [ "tag" .= ("ThreadPostResponses" :: Text)
    , "thread_post_responses" .= threadPostResponses
    ]


instance Eq ThreadPostResponses where
  (==) a b = threadPostResponses a == threadPostResponses b

instance Show ThreadPostResponses where
    show rec = "threadPostResponses: " <> show (threadPostResponses rec)

data ThreadPostStatResponse = ThreadPostStatResponse {
  threadPostStatResponseThreadPostId :: !(Int64),
  threadPostStatResponseLikes :: !(Int64),
  threadPostStatResponseNeutral :: !(Int64),
  threadPostStatResponseDislikes :: !(Int64),
  threadPostStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostStatResponse where
  parseJSON (Object o) = do
    threadPostStatResponseThreadPostId <- o .: ("thread_post_id")
    threadPostStatResponseLikes <- o .: ("likes")
    threadPostStatResponseNeutral <- o .: ("neutral")
    threadPostStatResponseDislikes <- o .: ("dislikes")
    threadPostStatResponseViews <- o .: ("views")
    pure $ ThreadPostStatResponse {
      threadPostStatResponseThreadPostId = threadPostStatResponseThreadPostId,
      threadPostStatResponseLikes = threadPostStatResponseLikes,
      threadPostStatResponseNeutral = threadPostStatResponseNeutral,
      threadPostStatResponseDislikes = threadPostStatResponseDislikes,
      threadPostStatResponseViews = threadPostStatResponseViews
    }
  parseJSON x = fail $ "ThreadPostStatResponse: Could not parse object: " <> show x


instance ToJSON ThreadPostStatResponse where
  toJSON ThreadPostStatResponse{..} = object $
    [ "tag" .= ("ThreadPostStatResponse" :: Text)
    , "thread_post_id" .= threadPostStatResponseThreadPostId
    , "likes" .= threadPostStatResponseLikes
    , "neutral" .= threadPostStatResponseNeutral
    , "dislikes" .= threadPostStatResponseDislikes
    , "views" .= threadPostStatResponseViews
    ]


instance Eq ThreadPostStatResponse where
  (==) a b = threadPostStatResponseThreadPostId a == threadPostStatResponseThreadPostId b && threadPostStatResponseLikes a == threadPostStatResponseLikes b && threadPostStatResponseNeutral a == threadPostStatResponseNeutral b && threadPostStatResponseDislikes a == threadPostStatResponseDislikes b && threadPostStatResponseViews a == threadPostStatResponseViews b

instance Show ThreadPostStatResponse where
    show rec = "threadPostStatResponseThreadPostId: " <> show (threadPostStatResponseThreadPostId rec) <> ", " <> "threadPostStatResponseLikes: " <> show (threadPostStatResponseLikes rec) <> ", " <> "threadPostStatResponseNeutral: " <> show (threadPostStatResponseNeutral rec) <> ", " <> "threadPostStatResponseDislikes: " <> show (threadPostStatResponseDislikes rec) <> ", " <> "threadPostStatResponseViews: " <> show (threadPostStatResponseViews rec)

data ThreadPostStatResponses = ThreadPostStatResponses {
  threadPostStatResponses :: !([ThreadPostStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON ThreadPostStatResponses where
  parseJSON (Object o) = do
    threadPostStatResponses <- o .: ("thread_post_stat_responses")
    pure $ ThreadPostStatResponses {
      threadPostStatResponses = threadPostStatResponses
    }
  parseJSON x = fail $ "ThreadPostStatResponses: Could not parse object: " <> show x


instance ToJSON ThreadPostStatResponses where
  toJSON ThreadPostStatResponses{..} = object $
    [ "tag" .= ("ThreadPostStatResponses" :: Text)
    , "thread_post_stat_responses" .= threadPostStatResponses
    ]


instance Eq ThreadPostStatResponses where
  (==) a b = threadPostStatResponses a == threadPostStatResponses b

instance Show ThreadPostStatResponses where
    show rec = "threadPostStatResponses: " <> show (threadPostStatResponses rec)
-- footer
