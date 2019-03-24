{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LN.T.Convert where




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

import LN.T

apiRequestToApiResponse :: Int64 -> Int64 -> Text -> (Maybe UTCTime) -> (Maybe UTCTime) -> ApiRequest -> ApiResponse
apiRequestToApiResponse _1 _2 _3 _4 _5 ApiRequest{..} =
  ApiResponse {
    apiResponseId = _1,
    apiResponseUserId = _2,
    apiResponseKey = _3,
    apiResponseCreatedAt = _4,
    apiResponseModifiedAt = _5,
    apiResponseComment = apiRequestComment,
    apiResponseGuard = apiRequestGuard
  }


apiResponseToApiRequest :: ApiResponse -> ApiRequest
apiResponseToApiRequest  ApiResponse{..} =
  ApiRequest {
    apiRequestComment = apiResponseComment,
    apiRequestGuard = apiResponseGuard
  }


profileRequestToProfileResponse :: Int64 -> Ent -> Int64 -> Int -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> ProfileRequest -> ProfileResponse
profileRequestToProfileResponse _1 _2 _3 _4 _5 _6 _7 ProfileRequest{..} =
  ProfileResponse {
    profileResponseId = _1,
    profileResponseEnt = _2,
    profileResponseEntId = _3,
    profileResponseKarmaGood = _4,
    profileResponseKarmaBad = _5,
    profileResponseCreatedAt = _6,
    profileResponseModifiedAt = _7,
    profileResponseGender = profileRequestGender,
    profileResponseBirthdate = profileRequestBirthdate,
    profileResponseWebsite = profileRequestWebsite,
    profileResponseLocation = profileRequestLocation,
    profileResponseSignature = profileRequestSignature,
    profileResponseDebug = profileRequestDebug,
    profileResponseGuard = profileRequestGuard
  }


profileResponseToProfileRequest :: [Text] -> (Maybe Text) -> ProfileResponse -> ProfileRequest
profileResponseToProfileRequest _1 _2 ProfileResponse{..} =
  ProfileRequest {
    profileRequestWebsites = _1,
    profileRequestStateWebsites = _2,
    profileRequestGender = profileResponseGender,
    profileRequestBirthdate = profileResponseBirthdate,
    profileRequestWebsite = profileResponseWebsite,
    profileRequestLocation = profileResponseLocation,
    profileRequestSignature = profileResponseSignature,
    profileRequestDebug = profileResponseDebug,
    profileRequestGuard = profileResponseGuard
  }


forumRequestToForumResponse :: Int64 -> Int64 -> Text -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ForumRequest -> ForumResponse
forumRequestToForumResponse _1 _2 _3 _4 _5 _6 _7 ForumRequest{..} =
  ForumResponse {
    forumResponseId = _1,
    forumResponseUserId = _2,
    forumResponseName = _3,
    forumResponseCreatedAt = _4,
    forumResponseModifiedBy = _5,
    forumResponseModifiedAt = _6,
    forumResponseActivityAt = _7,
    forumResponseDisplayName = forumRequestDisplayName,
    forumResponseDescription = forumRequestDescription,
    forumResponseThreadsPerBoard = forumRequestThreadsPerBoard,
    forumResponseThreadPostsPerThread = forumRequestThreadPostsPerThread,
    forumResponseRecentThreadsLimit = forumRequestRecentThreadsLimit,
    forumResponseRecentPostsLimit = forumRequestRecentPostsLimit,
    forumResponseMotwLimit = forumRequestMotwLimit,
    forumResponseIcon = forumRequestIcon,
    forumResponseVisibility = forumRequestVisibility,
    forumResponseGuard = forumRequestGuard
  }


forumResponseToForumRequest :: [Text] -> (Maybe Text) -> ForumResponse -> ForumRequest
forumResponseToForumRequest _1 _2 ForumResponse{..} =
  ForumRequest {
    forumRequestTags = _1,
    forumRequestStateTag = _2,
    forumRequestDisplayName = forumResponseDisplayName,
    forumRequestDescription = forumResponseDescription,
    forumRequestThreadsPerBoard = forumResponseThreadsPerBoard,
    forumRequestThreadPostsPerThread = forumResponseThreadPostsPerThread,
    forumRequestRecentThreadsLimit = forumResponseRecentThreadsLimit,
    forumRequestRecentPostsLimit = forumResponseRecentPostsLimit,
    forumRequestMotwLimit = forumResponseMotwLimit,
    forumRequestIcon = forumResponseIcon,
    forumRequestVisibility = forumResponseVisibility,
    forumRequestGuard = forumResponseGuard
  }


boardRequestToBoardResponse :: Int64 -> Int64 -> Text -> Text -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse _1 _2 _3 _4 _5 _6 _7 BoardRequest{..} =
  BoardResponse {
    boardResponseId = _1,
    boardResponseUserId = _2,
    boardResponseName = _3,
    boardResponseDescription = _4,
    boardResponseCreatedAt = _5,
    boardResponseModifiedAt = _6,
    boardResponseActivityAt = _7,
    boardResponseDisplayName = boardRequestDisplayName,
    boardResponseBoardType = boardRequestBoardType,
    boardResponseActive = boardRequestActive,
    boardResponseIsAnonymous = boardRequestIsAnonymous,
    boardResponseCanCreateBoards = boardRequestCanCreateBoards,
    boardResponseCanCreateThreads = boardRequestCanCreateThreads,
    boardResponseVisibility = boardRequestVisibility,
    boardResponseIcon = boardRequestIcon,
    boardResponseTags = boardRequestTags,
    boardResponseGuard = boardRequestGuard
  }


boardResponseToBoardRequest :: (Maybe Text) -> BoardResponse -> BoardRequest
boardResponseToBoardRequest _1 BoardResponse{..} =
  BoardRequest {
    boardRequestDescription = _1,
    boardRequestDisplayName = boardResponseDisplayName,
    boardRequestBoardType = boardResponseBoardType,
    boardRequestActive = boardResponseActive,
    boardRequestIsAnonymous = boardResponseIsAnonymous,
    boardRequestCanCreateBoards = boardResponseCanCreateBoards,
    boardRequestCanCreateThreads = boardResponseCanCreateThreads,
    boardRequestVisibility = boardResponseVisibility,
    boardRequestIcon = boardResponseIcon,
    boardRequestTags = boardResponseTags,
    boardRequestGuard = boardResponseGuard
  }


threadRequestToThreadResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Text -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadRequest -> ThreadResponse
threadRequestToThreadResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 ThreadRequest{..} =
  ThreadResponse {
    threadResponseId = _1,
    threadResponseUserId = _2,
    threadResponseOrgId = _3,
    threadResponseForumId = _4,
    threadResponseBoardId = _5,
    threadResponseName = _6,
    threadResponseActive = _7,
    threadResponseCreatedAt = _8,
    threadResponseModifiedBy = _9,
    threadResponseModifiedAt = _10,
    threadResponseActivityAt = _11,
    threadResponseDisplayName = threadRequestDisplayName,
    threadResponseDescription = threadRequestDescription,
    threadResponseSticky = threadRequestSticky,
    threadResponseLocked = threadRequestLocked,
    threadResponsePoll = threadRequestPoll,
    threadResponseIcon = threadRequestIcon,
    threadResponseTags = threadRequestTags,
    threadResponseGuard = threadRequestGuard
  }


threadResponseToThreadRequest :: (Maybe Text) -> ThreadResponse -> ThreadRequest
threadResponseToThreadRequest _1 ThreadResponse{..} =
  ThreadRequest {
    threadRequestStateTag = _1,
    threadRequestDisplayName = threadResponseDisplayName,
    threadRequestDescription = threadResponseDescription,
    threadRequestSticky = threadResponseSticky,
    threadRequestLocked = threadResponseLocked,
    threadRequestPoll = threadResponsePoll,
    threadRequestIcon = threadResponseIcon,
    threadRequestTags = threadResponseTags,
    threadRequestGuard = threadResponseGuard
  }


threadPostRequestToThreadPostResponse :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> (Maybe Int64) -> Bool -> (Maybe UTCTime) -> (Maybe Int64) -> (Maybe UTCTime) -> (Maybe UTCTime) -> ThreadPostRequest -> ThreadPostResponse
threadPostRequestToThreadPostResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 ThreadPostRequest{..} =
  ThreadPostResponse {
    threadPostResponseId = _1,
    threadPostResponseUserId = _2,
    threadPostResponseOrgId = _3,
    threadPostResponseForumId = _4,
    threadPostResponseBoardId = _5,
    threadPostResponseThreadId = _6,
    threadPostResponseParentId = _7,
    threadPostResponseActive = _8,
    threadPostResponseCreatedAt = _9,
    threadPostResponseModifiedBy = _10,
    threadPostResponseModifiedAt = _11,
    threadPostResponseActivityAt = _12,
    threadPostResponseTitle = threadPostRequestTitle,
    threadPostResponseBody = threadPostRequestBody,
    threadPostResponseTags = threadPostRequestTags,
    threadPostResponsePrivateTags = threadPostRequestPrivateTags,
    threadPostResponseGuard = threadPostRequestGuard
  }


threadPostResponseToThreadPostRequest :: (Maybe Text) -> (Maybe Text) -> ThreadPostResponse -> ThreadPostRequest
threadPostResponseToThreadPostRequest _1 _2 ThreadPostResponse{..} =
  ThreadPostRequest {
    threadPostRequestStateTag = _1,
    threadPostRequestStatePrivateTag = _2,
    threadPostRequestTitle = threadPostResponseTitle,
    threadPostRequestBody = threadPostResponseBody,
    threadPostRequestTags = threadPostResponseTags,
    threadPostRequestPrivateTags = threadPostResponsePrivateTags,
    threadPostRequestGuard = threadPostResponseGuard
  }


userRequestToUserResponse :: Int64 -> Text -> Text -> (Maybe Text) -> (Maybe UTCTime) -> (Maybe Text) -> (Maybe UTCTime) -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserResponse
userRequestToUserResponse _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 UserRequest{..} =
  UserResponse {
    userResponseId = _1,
    userResponseName = _2,
    userResponseEmailMD5 = _3,
    userResponseGithubIdent = _4,
    userResponseGithubCreatedAt = _5,
    userResponseGoogleIdent = _6,
    userResponseGoogleCreatedAt = _7,
    userResponseActive = _8,
    userResponseGuard = _9,
    userResponseCreatedAt = _10,
    userResponseModifiedAt = _11,
    userResponseDeactivatedAt = _12,
    userResponseActivityAt = _13,
    userResponseDisplayName = userRequestDisplayName,
    userResponseFullName = userRequestFullName,
    userResponseEmail = userRequestEmail,
    userResponsePlugin = userRequestPlugin,
    userResponseAcceptTOS = userRequestAcceptTOS
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  UserResponse{..} =
  UserRequest {
    userRequestDisplayName = userResponseDisplayName,
    userRequestFullName = userResponseFullName,
    userRequestEmail = userResponseEmail,
    userRequestPlugin = userResponsePlugin,
    userRequestAcceptTOS = userResponseAcceptTOS
  }


userRequestToUserSanitizedResponse :: Int64 -> Text -> Text -> Bool -> Int -> (Maybe UTCTime) -> (Maybe UTCTime) -> UserRequest -> UserSanitizedResponse
userRequestToUserSanitizedResponse _1 _2 _3 _4 _5 _6 _7 UserRequest{..} =
  UserSanitizedResponse {
    userSanitizedResponseId = _1,
    userSanitizedResponseName = _2,
    userSanitizedResponseEmailMD5 = _3,
    userSanitizedResponseActive = _4,
    userSanitizedResponseGuard = _5,
    userSanitizedResponseCreatedAt = _6,
    userSanitizedResponseActivityAt = _7,
    userSanitizedResponseDisplayName = userRequestDisplayName
  }


userSanitizedResponseToUserRequest :: Text -> Text -> Text -> (Maybe UTCTime) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest _1 _2 _3 _4 UserSanitizedResponse{..} =
  UserRequest {
    userRequestFullName = _1,
    userRequestEmail = _2,
    userRequestPlugin = _3,
    userRequestAcceptTOS = _4,
    userRequestDisplayName = userSanitizedResponseDisplayName
  }

-- footer