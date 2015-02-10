{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.Types ( Member(..)
                             , MemberWithKey(..)
                             , Team(..)
                             , BearerToken(..)
                             , StatusInTeam(..)
                             , EmptyBody(..)
                             , DidDelete(..)
                             , PublicKey(..)
                             , PublicKeyFingerprint(..)
                             , TeamCreateRequest(..)
                             , GitHubReturnStatus(..)
                             , runGitHub
                             , runGitHub'
                             , GitHub) where
import Control.Applicative
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Network.HTTP.Client
import Network.Wreq.Types
import System.Environment.Compat (lookupEnv)
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T

-- | Represents a user in GitHub. Contains no more than login and user ID
data Member =
  Member { memberLogin :: T.Text
         , memberId    :: Integer
         } deriving (Show, Eq)

-- | Represents a team in GitHub. Contains the team's ID, the team's name and an optional description
data Team =
  Team { teamId          :: Integer
       , teamName        :: T.Text
       , teamDescription :: Maybe T.Text
       } deriving (Show, Eq)

-- | Represents a request to create a new team within an organization. The rest of the paramaters
--   are passed in the URL. Refer to <https://developer.github.com/v3/orgs/teams/#create-team>
data TeamCreateRequest =
  TeamCreateRequest { newTeamName        :: T.Text
                    , newTeamDescription :: T.Text
                    } deriving (Show, Eq)

-- | Represents a GitHub user with its public keys and fingerprints. A GitHub user might or might not
--   have any public keys
data MemberWithKey =
  MemberWithKey { member               :: Member
                , memberKey            :: [PublicKey]
                , memberKeyFingerprint :: [PublicKeyFingerprint]
                } deriving (Show, Eq)

-- | Represents a PublicKey within GitHub. It includes its ID and the public key encoded as base 64
data PublicKey =
  PublicKey { publicKeyId :: Integer
            , publicKey   :: T.Text
            } deriving (Show, Eq)

-- | Represents a Fingerprint. The `fingerprintId` field should match the fingerprint's public key ID
--   within GitHub
data PublicKeyFingerprint =
  PublicKeyFingerprint { fingerprintId        :: Integer
                       , publicKeyFingerprint :: T.Text
                       } deriving (Show, Eq)

-- | Some Wreq functions expect a body, but often GitHub's API will request no body. The PUT verb
--   and its implementation in Wreq is an example of this.
data EmptyBody = EmptyBody deriving (Show, Eq)

-- | When adding a user to a team GitHub will add it immediately if the user already belongs to the
--   to the organization the team is in. Otherwise it will send an email for the user to accept the
--   request to join the team. Functions related adding or removing teams will return either Active
--   or Pending correspondingly.
data StatusInTeam = Active  | Pending deriving (Show, Eq)


-- | Sum type to represent the success or failure of deletion of a resource within GitHub's API
data DidDelete = Deleted | NotDeleted deriving (Show, Eq)

instance FromJSON PublicKey where
  parseJSON (Object o) = PublicKey <$> o .: "id" <*> o .: "key"
  parseJSON _          = fail "Could not find public keys in document"

instance FromJSON StatusInTeam where
  parseJSON (Object o) =
    case HS.lookup "state" o of
      Just "active"  -> pure Active
      Just "pending" -> pure Pending
      Just _         -> fail "\"state\" key not \"active\" or \"pending\""
      Nothing        -> (fail . maybe "No error message from GitHub" show) (HS.lookup "message" o)
  parseJSON _         = fail "Expected a membership document, got something else"

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6 . map toLower } ''Member)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 4 . map toLower } ''Team)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 7 . map toLower } ''TeamCreateRequest)

-- | Error codes GitHub might return when attempting to use an API endpoint
data GitHubReturnStatus =   InvalidJSON             -- ^ GitHub could not parse the JSON document sent
                          | ValidationFailed        -- ^ Validation failed, an example of this error
                                                    --   is trying to create teams with the same name
                                                    --   within one organization
                          | InternalError           -- ^ In case GitHub returns 500 Internal Server Error
                                                    --   to some request
                          | NotFound                -- ^ When a resource has not been found. It does not
                                                    --   imply the resource does not exist
                          | NotAllowed              -- ^ Usually returned after GitHub replies with 403 Forbidden.
                                                    --   The user might not have permission to access/modify
                                                    --   that resource
                          | AllOk                   -- ^ This should never be returned
                          | RequiresAuthentication  -- ^ Accesing this resource requires authentication
                          | UnexpectedJSON String   -- ^ This library has failed to fulfill its purpose and could not
                                                    --   handle GitHub's response
                          deriving (Show, Eq)

-- | Instance that does not add anything to the body or headers of a PUT request
instance Putable EmptyBody where
  putPayload EmptyBody req = return $ req {requestBody = RequestBodyLBS ""}

instance Postable TeamCreateRequest where
  postPayload createRequest req = return $ req { requestBody = RequestBodyLBS (encode createRequest)}

-- | GitHub's OAuth 2.0 bearer token. This is simply added in an
--   Authorization header
newtype BearerToken = BearerToken { unBearerToken :: T.Text }

-- | The monad transformer where all operations run. Supports initial configuration
--   through a Reader monad and the possibility of failure through Either
type GitHub = EitherT GitHubReturnStatus (ReaderT BearerToken IO)

-- | Executes a computation built within the GitHub monad returning an Either within
--   the IO data type using the provided token
runGitHub' :: GitHub a -> BearerToken -> IO (Either GitHubReturnStatus a)
runGitHub' comp = runReaderT (runEitherT comp)

-- | Executes a computation built within the GitHub monad returning an Either within
--   the IO data type. Reads an API token from an environment variable named GITHUB_TOKEN
runGitHub :: GitHub a -> IO (Either GitHubReturnStatus a)
runGitHub comp = do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  case maybeToken of
    Just acquiredToken -> runGitHub' comp (BearerToken $ T.pack acquiredToken)
    Nothing            -> fail "Couldn't find GITHUB_TOKEN in environment"
