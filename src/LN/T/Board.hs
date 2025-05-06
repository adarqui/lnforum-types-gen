{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Board where


import LN.T.Visibility


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

data BoardType
  = FixMe 
  deriving (Generic,Typeable,NFData)


instance FromJSON BoardType where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Data.Aeson.Key.Key)
    case tag of
      ("FixMe" :: Text) -> do
        pure FixMe

      _ -> fail "Could not parse BoardType"

  parseJSON x = fail $ "BoardType: Could not parse object: " <> show x


instance ToJSON BoardType where
  toJSON (FixMe ) = object $
    [ "tag" .= ("FixMe" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq BoardType where
  (==) FixMe FixMe = True


instance Show BoardType where
  show FixMe = "fix_me"


data TyBoardType
  = TyFixMe 
  deriving (Generic,Typeable,NFData)


instance FromJSON TyBoardType where
  parseJSON (Object o) = do
    tag <- o .: ("tag" :: Data.Aeson.Key.Key)
    case tag of
      ("TyFixMe" :: Text) -> do
        pure TyFixMe

      _ -> fail "Could not parse TyBoardType"

  parseJSON x = fail $ "TyBoardType: Could not parse object: " <> show x


instance ToJSON TyBoardType where
  toJSON (TyFixMe ) = object $
    [ "tag" .= ("TyFixMe" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq TyBoardType where
  (==) TyFixMe TyFixMe = True


instance Show TyBoardType where
  show TyFixMe = "ty_fix_me"


data BoardRequest = BoardRequest {
  boardRequestDisplayName :: !(Text),
  boardRequestDescription :: !((Maybe Text)),
  boardRequestBoardType :: !(BoardType),
  boardRequestActive :: !(Bool),
  boardRequestIsAnonymous :: !(Bool),
  boardRequestCanCreateBoards :: !(Bool),
  boardRequestCanCreateThreads :: !(Bool),
  boardRequestVisibility :: !(Visibility),
  boardRequestIcon :: !((Maybe Text)),
  boardRequestTags :: !([Text]),
  boardRequestGuard :: !(Int)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardRequest where
  parseJSON (Object o) = do
    boardRequestDisplayName <- o .: ("display_name" :: Data.Aeson.Key.Key)
    boardRequestDescription <- o .: ("description" :: Data.Aeson.Key.Key)
    boardRequestBoardType <- o .: ("board_type" :: Data.Aeson.Key.Key)
    boardRequestActive <- o .: ("active" :: Data.Aeson.Key.Key)
    boardRequestIsAnonymous <- o .: ("is_anonymous" :: Data.Aeson.Key.Key)
    boardRequestCanCreateBoards <- o .: ("can_create_boards" :: Data.Aeson.Key.Key)
    boardRequestCanCreateThreads <- o .: ("can_create_threads" :: Data.Aeson.Key.Key)
    boardRequestVisibility <- o .: ("visibility" :: Data.Aeson.Key.Key)
    boardRequestIcon <- o .: ("icon" :: Data.Aeson.Key.Key)
    boardRequestTags <- o .: ("tags" :: Data.Aeson.Key.Key)
    boardRequestGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    pure $ BoardRequest {
      boardRequestDisplayName = boardRequestDisplayName,
      boardRequestDescription = boardRequestDescription,
      boardRequestBoardType = boardRequestBoardType,
      boardRequestActive = boardRequestActive,
      boardRequestIsAnonymous = boardRequestIsAnonymous,
      boardRequestCanCreateBoards = boardRequestCanCreateBoards,
      boardRequestCanCreateThreads = boardRequestCanCreateThreads,
      boardRequestVisibility = boardRequestVisibility,
      boardRequestIcon = boardRequestIcon,
      boardRequestTags = boardRequestTags,
      boardRequestGuard = boardRequestGuard
    }
  parseJSON x = fail $ "BoardRequest: Could not parse object: " <> show x


instance ToJSON BoardRequest where
  toJSON BoardRequest{..} = object $
    [ "tag" .= ("BoardRequest" :: Text)
    , "display_name" .= boardRequestDisplayName
    , "description" .= boardRequestDescription
    , "board_type" .= boardRequestBoardType
    , "active" .= boardRequestActive
    , "is_anonymous" .= boardRequestIsAnonymous
    , "can_create_boards" .= boardRequestCanCreateBoards
    , "can_create_threads" .= boardRequestCanCreateThreads
    , "visibility" .= boardRequestVisibility
    , "icon" .= boardRequestIcon
    , "tags" .= boardRequestTags
    , "guard" .= boardRequestGuard
    ]


instance Eq BoardRequest where
  (==) a b = boardRequestDisplayName a == boardRequestDisplayName b && boardRequestDescription a == boardRequestDescription b && boardRequestBoardType a == boardRequestBoardType b && boardRequestActive a == boardRequestActive b && boardRequestIsAnonymous a == boardRequestIsAnonymous b && boardRequestCanCreateBoards a == boardRequestCanCreateBoards b && boardRequestCanCreateThreads a == boardRequestCanCreateThreads b && boardRequestVisibility a == boardRequestVisibility b && boardRequestIcon a == boardRequestIcon b && boardRequestTags a == boardRequestTags b && boardRequestGuard a == boardRequestGuard b

instance Show BoardRequest where
    show rec = "boardRequestDisplayName: " <> show (boardRequestDisplayName rec) <> ", " <> "boardRequestDescription: " <> show (boardRequestDescription rec) <> ", " <> "boardRequestBoardType: " <> show (boardRequestBoardType rec) <> ", " <> "boardRequestActive: " <> show (boardRequestActive rec) <> ", " <> "boardRequestIsAnonymous: " <> show (boardRequestIsAnonymous rec) <> ", " <> "boardRequestCanCreateBoards: " <> show (boardRequestCanCreateBoards rec) <> ", " <> "boardRequestCanCreateThreads: " <> show (boardRequestCanCreateThreads rec) <> ", " <> "boardRequestVisibility: " <> show (boardRequestVisibility rec) <> ", " <> "boardRequestIcon: " <> show (boardRequestIcon rec) <> ", " <> "boardRequestTags: " <> show (boardRequestTags rec) <> ", " <> "boardRequestGuard: " <> show (boardRequestGuard rec)

data BoardResponse = BoardResponse {
  boardResponseId :: !(Int64),
  boardResponseUserId :: !(Int64),
  boardResponseName :: !(Text),
  boardResponseDisplayName :: !(Text),
  boardResponseDescription :: !((Maybe Text)),
  boardResponseBoardType :: !(BoardType),
  boardResponseActive :: !(Bool),
  boardResponseIsAnonymous :: !(Bool),
  boardResponseCanCreateBoards :: !(Bool),
  boardResponseCanCreateThreads :: !(Bool),
  boardResponseVisibility :: !(Visibility),
  boardResponseIcon :: !((Maybe Text)),
  boardResponseTags :: !([Text]),
  boardResponseGuard :: !(Int),
  boardResponseCreatedAt :: !((Maybe UTCTime)),
  boardResponseModifiedAt :: !((Maybe UTCTime)),
  boardResponseModifiedBy :: !((Maybe Int64)),
  boardResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponse where
  parseJSON (Object o) = do
    boardResponseId <- o .: ("id" :: Data.Aeson.Key.Key)
    boardResponseUserId <- o .: ("user_id" :: Data.Aeson.Key.Key)
    boardResponseName <- o .: ("name" :: Data.Aeson.Key.Key)
    boardResponseDisplayName <- o .: ("display_name" :: Data.Aeson.Key.Key)
    boardResponseDescription <- o .: ("description" :: Data.Aeson.Key.Key)
    boardResponseBoardType <- o .: ("board_type" :: Data.Aeson.Key.Key)
    boardResponseActive <- o .: ("active" :: Data.Aeson.Key.Key)
    boardResponseIsAnonymous <- o .: ("is_anonymous" :: Data.Aeson.Key.Key)
    boardResponseCanCreateBoards <- o .: ("can_create_boards" :: Data.Aeson.Key.Key)
    boardResponseCanCreateThreads <- o .: ("can_create_threads" :: Data.Aeson.Key.Key)
    boardResponseVisibility <- o .: ("visibility" :: Data.Aeson.Key.Key)
    boardResponseIcon <- o .: ("icon" :: Data.Aeson.Key.Key)
    boardResponseTags <- o .: ("tags" :: Data.Aeson.Key.Key)
    boardResponseGuard <- o .: ("guard" :: Data.Aeson.Key.Key)
    boardResponseCreatedAt <- o .: ("created_at" :: Data.Aeson.Key.Key)
    boardResponseModifiedAt <- o .: ("modified_at" :: Data.Aeson.Key.Key)
    boardResponseModifiedBy <- o .: ("modified_by" :: Data.Aeson.Key.Key)
    boardResponseActivityAt <- o .: ("activity_at" :: Data.Aeson.Key.Key)
    pure $ BoardResponse {
      boardResponseId = boardResponseId,
      boardResponseUserId = boardResponseUserId,
      boardResponseName = boardResponseName,
      boardResponseDisplayName = boardResponseDisplayName,
      boardResponseDescription = boardResponseDescription,
      boardResponseBoardType = boardResponseBoardType,
      boardResponseActive = boardResponseActive,
      boardResponseIsAnonymous = boardResponseIsAnonymous,
      boardResponseCanCreateBoards = boardResponseCanCreateBoards,
      boardResponseCanCreateThreads = boardResponseCanCreateThreads,
      boardResponseVisibility = boardResponseVisibility,
      boardResponseIcon = boardResponseIcon,
      boardResponseTags = boardResponseTags,
      boardResponseGuard = boardResponseGuard,
      boardResponseCreatedAt = boardResponseCreatedAt,
      boardResponseModifiedAt = boardResponseModifiedAt,
      boardResponseModifiedBy = boardResponseModifiedBy,
      boardResponseActivityAt = boardResponseActivityAt
    }
  parseJSON x = fail $ "BoardResponse: Could not parse object: " <> show x


instance ToJSON BoardResponse where
  toJSON BoardResponse{..} = object $
    [ "tag" .= ("BoardResponse" :: Text)
    , "id" .= boardResponseId
    , "user_id" .= boardResponseUserId
    , "name" .= boardResponseName
    , "display_name" .= boardResponseDisplayName
    , "description" .= boardResponseDescription
    , "board_type" .= boardResponseBoardType
    , "active" .= boardResponseActive
    , "is_anonymous" .= boardResponseIsAnonymous
    , "can_create_boards" .= boardResponseCanCreateBoards
    , "can_create_threads" .= boardResponseCanCreateThreads
    , "visibility" .= boardResponseVisibility
    , "icon" .= boardResponseIcon
    , "tags" .= boardResponseTags
    , "guard" .= boardResponseGuard
    , "created_at" .= boardResponseCreatedAt
    , "modified_at" .= boardResponseModifiedAt
    , "modified_by" .= boardResponseModifiedBy
    , "activity_at" .= boardResponseActivityAt
    ]


instance Eq BoardResponse where
  (==) a b = boardResponseId a == boardResponseId b && boardResponseUserId a == boardResponseUserId b && boardResponseName a == boardResponseName b && boardResponseDisplayName a == boardResponseDisplayName b && boardResponseDescription a == boardResponseDescription b && boardResponseBoardType a == boardResponseBoardType b && boardResponseActive a == boardResponseActive b && boardResponseIsAnonymous a == boardResponseIsAnonymous b && boardResponseCanCreateBoards a == boardResponseCanCreateBoards b && boardResponseCanCreateThreads a == boardResponseCanCreateThreads b && boardResponseVisibility a == boardResponseVisibility b && boardResponseIcon a == boardResponseIcon b && boardResponseTags a == boardResponseTags b && boardResponseGuard a == boardResponseGuard b && boardResponseCreatedAt a == boardResponseCreatedAt b && boardResponseModifiedAt a == boardResponseModifiedAt b && boardResponseModifiedBy a == boardResponseModifiedBy b && boardResponseActivityAt a == boardResponseActivityAt b

instance Show BoardResponse where
    show rec = "boardResponseId: " <> show (boardResponseId rec) <> ", " <> "boardResponseUserId: " <> show (boardResponseUserId rec) <> ", " <> "boardResponseName: " <> show (boardResponseName rec) <> ", " <> "boardResponseDisplayName: " <> show (boardResponseDisplayName rec) <> ", " <> "boardResponseDescription: " <> show (boardResponseDescription rec) <> ", " <> "boardResponseBoardType: " <> show (boardResponseBoardType rec) <> ", " <> "boardResponseActive: " <> show (boardResponseActive rec) <> ", " <> "boardResponseIsAnonymous: " <> show (boardResponseIsAnonymous rec) <> ", " <> "boardResponseCanCreateBoards: " <> show (boardResponseCanCreateBoards rec) <> ", " <> "boardResponseCanCreateThreads: " <> show (boardResponseCanCreateThreads rec) <> ", " <> "boardResponseVisibility: " <> show (boardResponseVisibility rec) <> ", " <> "boardResponseIcon: " <> show (boardResponseIcon rec) <> ", " <> "boardResponseTags: " <> show (boardResponseTags rec) <> ", " <> "boardResponseGuard: " <> show (boardResponseGuard rec) <> ", " <> "boardResponseCreatedAt: " <> show (boardResponseCreatedAt rec) <> ", " <> "boardResponseModifiedAt: " <> show (boardResponseModifiedAt rec) <> ", " <> "boardResponseModifiedBy: " <> show (boardResponseModifiedBy rec) <> ", " <> "boardResponseActivityAt: " <> show (boardResponseActivityAt rec)

data BoardResponses = BoardResponses {
  boardResponses :: !([BoardResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponses where
  parseJSON (Object o) = do
    boardResponses <- o .: ("board_responses" :: Data.Aeson.Key.Key)
    pure $ BoardResponses {
      boardResponses = boardResponses
    }
  parseJSON x = fail $ "BoardResponses: Could not parse object: " <> show x


instance ToJSON BoardResponses where
  toJSON BoardResponses{..} = object $
    [ "tag" .= ("BoardResponses" :: Text)
    , "board_responses" .= boardResponses
    ]


instance Eq BoardResponses where
  (==) a b = boardResponses a == boardResponses b

instance Show BoardResponses where
    show rec = "boardResponses: " <> show (boardResponses rec)

data BoardStatResponse = BoardStatResponse {
  boardStatResponseBoardId :: !(Int64),
  boardStatResponseThreads :: !(Int64),
  boardStatResponseThreadPosts :: !(Int64),
  boardStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponse where
  parseJSON (Object o) = do
    boardStatResponseBoardId <- o .: ("board_id" :: Data.Aeson.Key.Key)
    boardStatResponseThreads <- o .: ("threads" :: Data.Aeson.Key.Key)
    boardStatResponseThreadPosts <- o .: ("thread_posts" :: Data.Aeson.Key.Key)
    boardStatResponseViews <- o .: ("views" :: Data.Aeson.Key.Key)
    pure $ BoardStatResponse {
      boardStatResponseBoardId = boardStatResponseBoardId,
      boardStatResponseThreads = boardStatResponseThreads,
      boardStatResponseThreadPosts = boardStatResponseThreadPosts,
      boardStatResponseViews = boardStatResponseViews
    }
  parseJSON x = fail $ "BoardStatResponse: Could not parse object: " <> show x


instance ToJSON BoardStatResponse where
  toJSON BoardStatResponse{..} = object $
    [ "tag" .= ("BoardStatResponse" :: Text)
    , "board_id" .= boardStatResponseBoardId
    , "threads" .= boardStatResponseThreads
    , "thread_posts" .= boardStatResponseThreadPosts
    , "views" .= boardStatResponseViews
    ]


instance Eq BoardStatResponse where
  (==) a b = boardStatResponseBoardId a == boardStatResponseBoardId b && boardStatResponseThreads a == boardStatResponseThreads b && boardStatResponseThreadPosts a == boardStatResponseThreadPosts b && boardStatResponseViews a == boardStatResponseViews b

instance Show BoardStatResponse where
    show rec = "boardStatResponseBoardId: " <> show (boardStatResponseBoardId rec) <> ", " <> "boardStatResponseThreads: " <> show (boardStatResponseThreads rec) <> ", " <> "boardStatResponseThreadPosts: " <> show (boardStatResponseThreadPosts rec) <> ", " <> "boardStatResponseViews: " <> show (boardStatResponseViews rec)

data BoardStatResponses = BoardStatResponses {
  boardStatResponses :: !([BoardStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponses where
  parseJSON (Object o) = do
    boardStatResponses <- o .: ("board_stat_responses" :: Data.Aeson.Key.Key)
    pure $ BoardStatResponses {
      boardStatResponses = boardStatResponses
    }
  parseJSON x = fail $ "BoardStatResponses: Could not parse object: " <> show x


instance ToJSON BoardStatResponses where
  toJSON BoardStatResponses{..} = object $
    [ "tag" .= ("BoardStatResponses" :: Text)
    , "board_stat_responses" .= boardStatResponses
    ]


instance Eq BoardStatResponses where
  (==) a b = boardStatResponses a == boardStatResponses b

instance Show BoardStatResponses where
    show rec = "boardStatResponses: " <> show (boardStatResponses rec)
-- footer
