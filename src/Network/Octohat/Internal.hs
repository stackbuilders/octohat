{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Octohat.Internal
  ( putRequestTo
  , getRequestTo
  , postRequestTo
  , deleteRequestTo
  , composeEndpoint) where

import Control.Error.Safe
import Control.Lens (set, view)
import Control.Monad.Reader
import Data.Aeson
import Data.List
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq
import qualified Network.Wreq.Types as WT
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Network.Octohat.Types

composeEndpoint :: [T.Text] -> T.Text
composeEndpoint pathChunks = T.concat $ intersperse "/" ("https://api.github.com" : pathChunks)

getResponseEntity :: FromJSON a => Response BSL.ByteString -> Either GitHubReturnStatus a
getResponseEntity resp =
  case eitherDecode (view responseBody resp) of
    Left errorMessage -> Left (UnexpectedJSON errorMessage)
    Right decoded     -> Right decoded

requestOptions :: GitHub Options
requestOptions = do
  bearerToken <- ask
  let opts   = set auth (Just $ oauth2Bearer (encodeUtf8 $ unBearerToken bearerToken)) defaults
  let opts'  = set checkStatus (Just (\_ _ _ -> Nothing)) opts
  let opts'' = set (header "User-Agent") ["octohat v0.1"] opts'
  return opts''


postRequestTo :: (ToJSON b, WT.Postable b, FromJSON a) => T.Text -> b -> GitHub a
postRequestTo uri body = do
  opts     <- requestOptions
  response <- liftIO $ postWith opts (T.unpack uri) (toJSON body)
  checkForStatus response
  tryRight $ getResponseEntity response

getRequestTo :: FromJSON a => T.Text -> GitHub a
getRequestTo uri = do
  opts     <- requestOptions
  response <- liftIO $ getWith opts (T.unpack uri)
  checkForStatus response
  tryRight $ getResponseEntity response

putRequestTo :: FromJSON a => T.Text -> GitHub a
putRequestTo uri = do
  opts     <- requestOptions
  response <- liftIO $ putWith opts (T.unpack uri) EmptyBody
  checkForStatus response
  tryRight $ getResponseEntity response

deleteRequestTo :: T.Text -> GitHub DidDelete
deleteRequestTo uri = do
  opts     <- requestOptions
  response <- liftIO $ deleteWith opts (T.unpack uri)
  checkForStatus response
  return $ isDeleted (viewResponse response)

checkForStatus :: Response a -> GitHub ()
checkForStatus (viewResponse -> 404) = tryAssert NotFound False
checkForStatus (viewResponse -> 403) = tryAssert NotAllowed False
checkForStatus (viewResponse -> 401) = tryAssert RequiresAuthentication False
checkForStatus (viewResponse -> 422) = tryAssert ValidationFailed False
checkForStatus (viewResponse -> 500) = tryAssert InternalError False
checkForStatus (viewResponse -> 400) = tryAssert InvalidJSON False
checkForStatus (viewResponse -> _)   = tryAssert AllOk True

viewResponse :: Response a -> Int
viewResponse = view (responseStatus . statusCode)

isDeleted :: Int -> DidDelete
isDeleted 204 = Deleted
isDeleted 200 = Deleted
isDeleted _   = NotDeleted
