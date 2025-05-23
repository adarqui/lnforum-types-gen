{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.ACL where


import LN.T.Permission


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

data ACL
  = ACL_Grant !(Permissions)
  | ACL_Deny 
  deriving (Generic,Typeable,NFData)


instance FromJSON ACL where
  parseJSON (Object o) = do
    tag <- o .: ("tag")
    case tag of
      ("ACL_Grant" :: Text) -> do
        r <- o .: "contents"
        case r of
          [x0] -> ACL_Grant <$> parseJSON x0
          _ -> fail "FromJON Typemismatch: ACL_Grant"

      ("ACL_Deny" :: Text) -> do
        pure ACL_Deny

      _ -> fail "Could not parse ACL"

  parseJSON x = fail $ "Could not parse object: " <> show x


instance ToJSON ACL where
  toJSON (ACL_Grant x0) = object $
    [ "tag" .= ("ACL_Grant" :: Text)
    , "contents" .= [toJSON x0]
    ]
  toJSON (ACL_Deny ) = object $
    [ "tag" .= ("ACL_Deny" :: Text)
    , "contents" .= ([] :: [Text])
    ]


instance Eq ACL where
  (==) (ACL_Grant x0a) (ACL_Grant x0b) = x0a == x0b
  (==) ACL_Deny ACL_Deny = True
  (==) _ _ = False

instance Show ACL where
  show (ACL_Grant x0) = "acl_grant: " <> show x0
  show ACL_Deny = "deny"

-- footer
