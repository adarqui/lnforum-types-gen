{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Forum where


import LN.T.Forum
import LN.T.User
import LN.T.Permission
import LN.T.Like


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

data ForumPackResponse = ForumPackResponse {
  forumPackResponseForum :: !(ForumResponse),
  forumPackResponseStat :: !(ForumStatResponse)
}  deriving (Generic,Typeable,NFData)


instance FromJSON ForumPackResponse where
  parseJSON (Object o) = do
    forumPackResponseForum <- o .: ("forum" :: Text)
    forumPackResponseStat <- o .: ("stat" :: Text)
    pure $ ForumPackResponse {
      forumPackResponseForum = forumPackResponseForum,
      forumPackResponseStat = forumPackResponseStat
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ForumPackResponse where
  toJSON ForumPackResponse{..} = object $
    [ "tag" .= ("ForumPackResponse" :: Text)
    , "forum" .= forumPackResponseForum
    , "stat" .= forumPackResponseStat
    ]


instance Eq ForumPackResponse where
  (==) a b = forumPackResponseForum a == forumPackResponseForum b && forumPackResponseStat a == forumPackResponseStat b

instance Show ForumPackResponse where
    show rec = "forumPackResponseForum: " <> show (forumPackResponseForum rec) <> ", " <> "forumPackResponseStat: " <> show (forumPackResponseStat rec)
-- footer