{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Pack.Boot where


import LN.T.Pack.User
import LN.T.Pack.Forum
import LN.T.Pack.Board
import LN.T.Pack.ThreadPost


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

data BootPackResponse = BootPackResponse {
  bootPackResponseForumPack :: !(ForumPackResponse),
  bootPackResponseBoardPacks :: !([BoardPackResponse]),
  bootPackResponseRecentPosts :: !([ThreadPostPackResponse]),
  bootPackResponsePostsOfTheWeek :: !([ThreadPostPackResponse]),
  bootPackResponseUsersOnline :: !([UserPackResponse])
}  deriving (Generic,Typeable,NFData)


instance FromJSON BootPackResponse where
  parseJSON (Object o) = do
    bootPackResponseForumPack <- o .: ("forum_pack")
    bootPackResponseBoardPacks <- o .: ("board_packs")
    bootPackResponseRecentPosts <- o .: ("recent_posts")
    bootPackResponsePostsOfTheWeek <- o .: ("posts_of_the_week")
    bootPackResponseUsersOnline <- o .: ("users_online")
    pure $ BootPackResponse {
      bootPackResponseForumPack = bootPackResponseForumPack,
      bootPackResponseBoardPacks = bootPackResponseBoardPacks,
      bootPackResponseRecentPosts = bootPackResponseRecentPosts,
      bootPackResponsePostsOfTheWeek = bootPackResponsePostsOfTheWeek,
      bootPackResponseUsersOnline = bootPackResponseUsersOnline
    }
  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON BootPackResponse where
  toJSON BootPackResponse{..} = object $
    [ "tag" .= ("BootPackResponse" :: Text)
    , "forum_pack" .= bootPackResponseForumPack
    , "board_packs" .= bootPackResponseBoardPacks
    , "recent_posts" .= bootPackResponseRecentPosts
    , "posts_of_the_week" .= bootPackResponsePostsOfTheWeek
    , "users_online" .= bootPackResponseUsersOnline
    ]


instance Eq BootPackResponse where
  (==) a b = bootPackResponseForumPack a == bootPackResponseForumPack b && bootPackResponseBoardPacks a == bootPackResponseBoardPacks b && bootPackResponseRecentPosts a == bootPackResponseRecentPosts b && bootPackResponsePostsOfTheWeek a == bootPackResponsePostsOfTheWeek b && bootPackResponseUsersOnline a == bootPackResponseUsersOnline b

instance Show BootPackResponse where
    show rec = "bootPackResponseForumPack: " <> show (bootPackResponseForumPack rec) <> ", " <> "bootPackResponseBoardPacks: " <> show (bootPackResponseBoardPacks rec) <> ", " <> "bootPackResponseRecentPosts: " <> show (bootPackResponseRecentPosts rec) <> ", " <> "bootPackResponsePostsOfTheWeek: " <> show (bootPackResponsePostsOfTheWeek rec) <> ", " <> "bootPackResponseUsersOnline: " <> show (bootPackResponseUsersOnline rec)
-- footer
