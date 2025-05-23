{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Job where


import LN.T


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

data Job
  = Job_Nop !(()) !(())
  | Job_Ping !(()) !(UTCTime)
  | Job_CreateUserProfile !((((,) Int64) ProfileRequest)) !(ProfileResponse)
  deriving (Generic,Typeable,NFData)


instance FromJSON Job where
  parseJSON (Object o) = do
    tag <- o .: ("tag")
    case tag of
      ("Job_Nop" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> Job_Nop <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: Job_Nop"

      ("Job_Ping" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> Job_Ping <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: Job_Ping"

      ("Job_CreateUserProfile" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0, x1] -> Job_CreateUserProfile <$> parseJSON x0 <*> parseJSON x1
          _ -> fail "FromJON Typemismatch: Job_CreateUserProfile"

      _ -> fail "Could not parse Job"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Job where
  toJSON (Job_Nop x0 x1) = object $
    [ "tag" .= ("Job_Nop" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (Job_Ping x0 x1) = object $
    [ "tag" .= ("Job_Ping" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]
  toJSON (Job_CreateUserProfile x0 x1) = object $
    [ "tag" .= ("Job_CreateUserProfile" :: Text)
    , "contents" .= [toJSON x0, toJSON x1]
    ]


instance Eq Job where
  (==) (Job_Nop x0a x1a) (Job_Nop x0b x1b) = x0a == x0b && x1a == x1b
  (==) (Job_Ping x0a x1a) (Job_Ping x0b x1b) = x0a == x0b && x1a == x1b
  (==) (Job_CreateUserProfile x0a x1a) (Job_CreateUserProfile x0b x1b) = x0a == x0b && x1a == x1b
  (==) _ _ = False

instance Show Job where
  show (Job_Nop x0 x1) = "job_nop: " <> show x0 <> " " <> show x1
  show (Job_Ping x0 x1) = "job_ping: " <> show x0 <> " " <> show x1
  show (Job_CreateUserProfile x0 x1) = "job_create_user_profile: " <> show x0 <> " " <> show x1


data Queue
  = QNop 
  | QPing 
  | QCreateUserProfile 
  | QCreateUserApi 
  | QAddThreadPostToSet 
  | QRemoveThreadPostFromSet 
  | QFixUserProfiles 
  | QFixThreadPostSets 
  deriving (Generic,Typeable,NFData)


instance FromJSON Queue where
  parseJSON (Object o) = do
    tag <- o .: ("tag")
    case tag of
      ("QNop" :: Text) -> do
        pure QNop

      ("QPing" :: Text) -> do
        pure QPing

      ("QCreateUserProfile" :: Text) -> do
        pure QCreateUserProfile

      ("QCreateUserApi" :: Text) -> do
        pure QCreateUserApi

      ("QAddThreadPostToSet" :: Text) -> do
        pure QAddThreadPostToSet

      ("QRemoveThreadPostFromSet" :: Text) -> do
        pure QRemoveThreadPostFromSet

      ("QFixUserProfiles" :: Text) -> do
        pure QFixUserProfiles

      ("QFixThreadPostSets" :: Text) -> do
        pure QFixThreadPostSets

      _ -> fail "Could not parse Queue"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON Queue where
  toJSON (QNop ) = object $
    [ "tag" .= ("QNop" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QPing ) = object $
    [ "tag" .= ("QPing" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QCreateUserProfile ) = object $
    [ "tag" .= ("QCreateUserProfile" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QCreateUserApi ) = object $
    [ "tag" .= ("QCreateUserApi" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QAddThreadPostToSet ) = object $
    [ "tag" .= ("QAddThreadPostToSet" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QRemoveThreadPostFromSet ) = object $
    [ "tag" .= ("QRemoveThreadPostFromSet" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QFixUserProfiles ) = object $
    [ "tag" .= ("QFixUserProfiles" :: Text)
    , "contents" .= ([] :: [Text])
    ]
  toJSON (QFixThreadPostSets ) = object $
    [ "tag" .= ("QFixThreadPostSets" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq Queue where
  (==) QNop QNop = True
  (==) QPing QPing = True
  (==) QCreateUserProfile QCreateUserProfile = True
  (==) QCreateUserApi QCreateUserApi = True
  (==) QAddThreadPostToSet QAddThreadPostToSet = True
  (==) QRemoveThreadPostFromSet QRemoveThreadPostFromSet = True
  (==) QFixUserProfiles QFixUserProfiles = True
  (==) QFixThreadPostSets QFixThreadPostSets = True
  (==) _ _ = False

instance Show Queue where
  show QNop = "qnop"
  show QPing = "qping"
  show QCreateUserProfile = "qcreate_user_profile"
  show QCreateUserApi = "qcreate_user_api"
  show QAddThreadPostToSet = "qadd_thread_post_to_set"
  show QRemoveThreadPostFromSet = "qremove_thread_post_from_set"
  show QFixUserProfiles = "qfix_user_profiles"
  show QFixThreadPostSets = "qfix_thread_post_sets"

-- footer
