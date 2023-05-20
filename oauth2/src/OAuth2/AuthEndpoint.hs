{-# LANGUAGE OverloadedStrings #-}
module OAuth2.AuthEndpoint
    (
        Request (..)
      , createRequestFromQuery
    ) where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as S

import qualified Network.HTTP.Types.URI as URI
import Data.List.Split

data Request = Request {
    responseType        :: S.ByteString
  , clientId            :: S.ByteString
  , redirectUri         :: S.ByteString
  , scope               :: [S.ByteString]
  , state               :: Maybe S.ByteString
  , codeChallenge       :: Maybe S.ByteString
  , codeChallengeMethod :: Maybe S.ByteString
} deriving (Show)

data Response = Response {
    code  :: S.ByteString
    state :: Maybe S.ByteString
}

parseScope :: Maybe S.ByteString -> [S.ByteString]
parseScope Nothing  = []
parseScope (Just s) = map S.pack $ splitOn (S.unpack $ S.singleton 58) (S.unpack s)

popQuery :: S.ByteString -> M.Map S.ByteString (Maybe S.ByteString) -> Maybe S.ByteString
popQuery key m = do
    value <- M.lookup key m
    value' <- value
    return value'

createRequestFromQuery :: URI.Query -> Maybe Request
createRequestFromQuery query = do
    responseType'        <- popQuery "response_type"         queryMap
    clientId'            <- popQuery "client_id"             queryMap
    redirectUri'         <- popQuery "redirect_uri"          queryMap
    return $ Request {
        responseType        = responseType'
      , clientId            = clientId'
      , redirectUri         = redirectUri'
      , scope               = parseScope scope'
      , state               = state'
      , codeChallenge       = codeChallenge'
      , codeChallengeMethod = codeChallengeMethod'
    }
    where
        queryMap = M.fromList $ query
        scope'               = popQuery "scope"                 queryMap
        state'               = popQuery "state"                 queryMap
        codeChallenge'       = popQuery "code_challenge"        queryMap
        codeChallengeMethod' = popQuery "code_challenge_method" queryMap
