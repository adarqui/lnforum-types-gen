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
    tag <- o .: ("tag" :: Text)
    case tag of
      ("FixMe" :: Text) -> do
        pure FixMe

      _ -> fail "Could not parse BoardType"

  parseJSON x = fail $ "Could not parse object: " <> show x


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
    tag <- o .: ("tag" :: Text)
    case tag of
      ("TyFixMe" :: Text) -> do
        pure TyFixMe

      _ -> fail "Could not parse TyBoardType"

  parseJSON x = fail $ "Could not parse object: " <> show x


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
    boardRequestDisplayName <- o .: ("display_name" :: Text)
    boardRequestDescription <- o .: ("description" :: Text)
    boardRequestBoardType <- o .: ("board_type" :: Text)
    boardRequestActive <- o .: ("active" :: Text)
    boardRequestIsAnonymous <- o .: ("is_anonymous" :: Text)
    boardRequestCanCreateBoards <- o .: ("can_create_boards" :: Text)
    boardRequestCanCreateThreads <- o .: ("can_create_threads" :: Text)
    boardRequestVisibility <- o .: ("visibility" :: Text)
    boardRequestIcon <- o .: ("icon" :: Text)
    boardRequestTags <- o .: ("tags" :: Text)
    boardRequestGuard <- o .: ("guard" :: Text)
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
  parseJSON x = fail $ "Could not parse object: " <> show x


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
  boardResponseDescription :: !(Text),
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
  boardResponseActivityAt :: !((Maybe UTCTime))
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponse where
  parseJSON (Object o) = do
    boardResponseId <- o .: ("id" :: Text)
    boardResponseUserId <- o .: ("user_id" :: Text)
    boardResponseName <- o .: ("name" :: Text)
    boardResponseDisplayName <- o .: ("display_name" :: Text)
    boardResponseDescription <- o .: ("description" :: Text)
    boardResponseBoardType <- o .: ("board_type" :: Text)
    boardResponseActive <- o .: ("active" :: Text)
    boardResponseIsAnonymous <- o .: ("is_anonymous" :: Text)
    boardResponseCanCreateBoards <- o .: ("can_create_boards" :: Text)
    boardResponseCanCreateThreads <- o .: ("can_create_threads" :: Text)
    boardResponseVisibility <- o .: ("visibility" :: Text)
    boardResponseIcon <- o .: ("icon" :: Text)
    boardResponseTags <- o .: ("tags" :: Text)
    boardResponseGuard <- o .: ("guard" :: Text)
    boardResponseCreatedAt <- o .: ("created_at" :: Text)
    boardResponseModifiedAt <- o .: ("modified_at" :: Text)
    boardResponseActivityAt <- o .: ("activity_at" :: Text)
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
      boardResponseActivityAt = boardResponseActivityAt
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


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
    , "activity_at" .= boardResponseActivityAt
    ]


instance Eq BoardResponse where
  (==) a b = boardResponseId a == boardResponseId b && boardResponseUserId a == boardResponseUserId b && boardResponseName a == boardResponseName b && boardResponseDisplayName a == boardResponseDisplayName b && boardResponseDescription a == boardResponseDescription b && boardResponseBoardType a == boardResponseBoardType b && boardResponseActive a == boardResponseActive b && boardResponseIsAnonymous a == boardResponseIsAnonymous b && boardResponseCanCreateBoards a == boardResponseCanCreateBoards b && boardResponseCanCreateThreads a == boardResponseCanCreateThreads b && boardResponseVisibility a == boardResponseVisibility b && boardResponseIcon a == boardResponseIcon b && boardResponseTags a == boardResponseTags b && boardResponseGuard a == boardResponseGuard b && boardResponseCreatedAt a == boardResponseCreatedAt b && boardResponseModifiedAt a == boardResponseModifiedAt b && boardResponseActivityAt a == boardResponseActivityAt b

instance Show BoardResponse where
    show rec = "boardResponseId: " <> show (boardResponseId rec) <> ", " <> "boardResponseUserId: " <> show (boardResponseUserId rec) <> ", " <> "boardResponseName: " <> show (boardResponseName rec) <> ", " <> "boardResponseDisplayName: " <> show (boardResponseDisplayName rec) <> ", " <> "boardResponseDescription: " <> show (boardResponseDescription rec) <> ", " <> "boardResponseBoardType: " <> show (boardResponseBoardType rec) <> ", " <> "boardResponseActive: " <> show (boardResponseActive rec) <> ", " <> "boardResponseIsAnonymous: " <> show (boardResponseIsAnonymous rec) <> ", " <> "boardResponseCanCreateBoards: " <> show (boardResponseCanCreateBoards rec) <> ", " <> "boardResponseCanCreateThreads: " <> show (boardResponseCanCreateThreads rec) <> ", " <> "boardResponseVisibility: " <> show (boardResponseVisibility rec) <> ", " <> "boardResponseIcon: " <> show (boardResponseIcon rec) <> ", " <> "boardResponseTags: " <> show (boardResponseTags rec) <> ", " <> "boardResponseGuard: " <> show (boardResponseGuard rec) <> ", " <> "boardResponseCreatedAt: " <> show (boardResponseCreatedAt rec) <> ", " <> "boardResponseModifiedAt: " <> show (boardResponseModifiedAt rec) <> ", " <> "boardResponseActivityAt: " <> show (boardResponseActivityAt rec)

data BoardResponses = BoardResponses {
  boardResponses :: !([BoardResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardResponses where
  parseJSON (Object o) = do
    boardResponses <- o .: ("board_responses" :: Text)
    pure $ BoardResponses {
      boardResponses = boardResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


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
  boardStatResponseLikes :: !(Int64),
  boardStatResponseNeutral :: !(Int64),
  boardStatResponseDislikes :: !(Int64),
  boardStatResponseViews :: !(Int64)
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponse where
  parseJSON (Object o) = do
    boardStatResponseBoardId <- o .: ("board_id" :: Text)
    boardStatResponseLikes <- o .: ("likes" :: Text)
    boardStatResponseNeutral <- o .: ("neutral" :: Text)
    boardStatResponseDislikes <- o .: ("dislikes" :: Text)
    boardStatResponseViews <- o .: ("views" :: Text)
    pure $ BoardStatResponse {
      boardStatResponseBoardId = boardStatResponseBoardId,
      boardStatResponseLikes = boardStatResponseLikes,
      boardStatResponseNeutral = boardStatResponseNeutral,
      boardStatResponseDislikes = boardStatResponseDislikes,
      boardStatResponseViews = boardStatResponseViews
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BoardStatResponse where
  toJSON BoardStatResponse{..} = object $
    [ "tag" .= ("BoardStatResponse" :: Text)
    , "board_id" .= boardStatResponseBoardId
    , "likes" .= boardStatResponseLikes
    , "neutral" .= boardStatResponseNeutral
    , "dislikes" .= boardStatResponseDislikes
    , "views" .= boardStatResponseViews
    ]


instance Eq BoardStatResponse where
  (==) a b = boardStatResponseBoardId a == boardStatResponseBoardId b && boardStatResponseLikes a == boardStatResponseLikes b && boardStatResponseNeutral a == boardStatResponseNeutral b && boardStatResponseDislikes a == boardStatResponseDislikes b && boardStatResponseViews a == boardStatResponseViews b

instance Show BoardStatResponse where
    show rec = "boardStatResponseBoardId: " <> show (boardStatResponseBoardId rec) <> ", " <> "boardStatResponseLikes: " <> show (boardStatResponseLikes rec) <> ", " <> "boardStatResponseNeutral: " <> show (boardStatResponseNeutral rec) <> ", " <> "boardStatResponseDislikes: " <> show (boardStatResponseDislikes rec) <> ", " <> "boardStatResponseViews: " <> show (boardStatResponseViews rec)

data BoardStatResponses = BoardStatResponses {
  boardStatResponses :: !([BoardStatResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BoardStatResponses where
  parseJSON (Object o) = do
    boardStatResponses <- o .: ("board_stat_responses" :: Text)
    pure $ BoardStatResponses {
      boardStatResponses = boardStatResponses
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


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