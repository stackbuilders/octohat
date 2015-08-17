{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Network.Octohat.Types ( Member(..)
                             , MemberWithKey(..)
                             , Team(..)
                             , TeamPermission(..)
                             , Repo(..)
                             , Organization(..)
                             , BearerToken(..)
                             , OrganizationName(..)
                             , TeamName(..)
                             , StatusInTeam(..)
                             , EmptyBody(..)
                             , DidDelete(..)
                             , PublicKey(..)
                             , PublicKeyFingerprint(..)
                             , TeamCreateRequest(..)
                             , GitHubReturnStatus(..)
                             , DidAddKey(..)
                             , AddPublicKeyRequest(..)
                             , Links(..)
                             , Pagination(..)
                             , runGitHub
                             , runGitHub'
                             , GitHub) where
import Control.Applicative
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..), evalStateT)
#if MIN_VERSION_errors(2,0,0)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
#else
import Control.Monad.Trans.Either
#endif
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

-- | Represents the different permissions that a team can have in an organisation.
data TeamPermission = OwnerAccess    -- ^ Default team of owners.
                    | PullAccess     -- ^ This team will be able to view and clone its
                                     --   repositories.
                    | PushAccess     -- ^ This team will be able to read its 
                                     --   repositories, as well as push to them.
                    | AdminAccess    -- ^ This team will be able to push/pull to its
                                     --   repositories, as well as add other 
                                     --   collaborators to them. 
                    deriving (Show,Eq)

-- | Represents a team in GitHub. Contains the team's ID, the team's name and an optional description
data Team =
  Team { teamId          :: Integer
       , teamName        :: T.Text
       , teamDescription :: Maybe T.Text
       , teamPermission  :: TeamPermission
       } deriving (Show, Eq)

-- | Represents a request to create a new team within an organization. The rest of the paramaters
--   are passed in the URL. Refer to <https://developer.github.com/v3/orgs/teams/#create-team>
data TeamCreateRequest =
  TeamCreateRequest { newTeamName        :: T.Text
                    , newTeamDescription :: T.Text
                    , newTeamPermission  :: TeamPermission
                    } deriving (Show, Eq)

-- | Represents an organisation in GitHub. Only has  name and description
data Organization =
  Organization 
       { orgLogin       :: T.Text
       , orgDescription :: Maybe T.Text
       } deriving (Show, Eq)

-- | Represents a repo in GitHub. Contains the Name, Description, and Private status
data Repo =
  Repo { repoName        :: T.Text
       , repoDescription :: Maybe T.Text
       , repoPrivate     :: Bool
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
  PublicKey { publicKeyId    :: Integer
            , publicKey      :: T.Text
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

data DidAddKey = KeyAdded | KeyNotAdded

data AddPublicKeyRequest =
  AddPublicKeyRequest {
      addPublicKeyRequestKey   :: T.Text,
      addPublicKeyRequestTitle :: T.Text
  }

instance FromJSON StatusInTeam where
  parseJSON (Object o) =
    case HS.lookup "state" o of
      Just "active"  -> pure Active
      Just "pending" -> pure Pending
      Just _         -> fail "\"state\" key not \"active\" or \"pending\""
      Nothing        -> (fail . maybe "No error message from GitHub" show) (HS.lookup "message" o)
  parseJSON _         = fail "Expected a membership document, got something else"

instance FromJSON TeamPermission where
  parseJSON (String p) = 
    case p of
      "pull"         -> pure PullAccess
      "push"         -> pure PushAccess
      "admin"        -> pure AdminAccess
      "owner"        -> pure OwnerAccess
      _              -> fail "Expected a valid team permission ?"
  parseJSON _         = fail "Expected a team permssion, got something else"

instance ToJSON TeamPermission where
  toJSON p = 
    case p of 
      PullAccess   -> String "pull"
      PushAccess   -> String "push"
      AdminAccess  -> String "admin"
      OwnerAccess  -> String "owner"


$(deriveJSON defaultOptions { fieldLabelModifier = drop 6  . map toLower } ''Member)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 4  . map toLower } ''Team)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 4  . map toLower } ''Repo)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 3  . map toLower } ''Organization)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 7  . map toLower } ''TeamCreateRequest)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 19 . map toLower } ''AddPublicKeyRequest)

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

instance Postable AddPublicKeyRequest where
  postPayload createRequest req = return $ req { requestBody = RequestBodyLBS (encode createRequest)}

-- | GitHub's OAuth 2.0 bearer token. This is simply added in an
--   Authorization header
newtype BearerToken = BearerToken { unBearerToken :: T.Text } deriving Show

-- | OrganizationName is added in order to have type safety in functions where the
--   Organization name and the Team name are both strings and may be confused
newtype OrganizationName = OrganizationName { unOrganizationName :: T.Text } deriving Show

-- | TeamName is added in order to have type safety in functions where the
--   Team name and the Organization name are both strings and may be confused
newtype TeamName = TeamName { unTeamName :: T.Text } deriving Show


-- | Links are used in the Pagination object
data Links = Links { linkNext  :: Maybe Link, linkLast :: Maybe Link
                   , linkFirst :: Maybe Link, linkPrev :: Maybe Link } deriving Show

-- | Pagination options that can be set, including the page number, and the per_page
data Pagination = Pagination { perPage :: Int, page :: Int, links :: Links, recurse :: Bool } deriving Show
defPagination :: Pagination
defPagination = Pagination 30 1 (Links Nothing Nothing Nothing Nothing) True

-- | The monad transformer where all operations run. Supports initial configuration
--   through a Reader monad and the possibility of failure through Either
#if MIN_VERSION_errors(2,0,0)
type GitHub = ExceptT GitHubReturnStatus (ReaderT BearerToken (StateT Pagination IO))
#else
type GitHub = EitherT GitHubReturnStatus (ReaderT BearerToken (StateT Pagination IO))
#endif

-- | Executes a computation built within the GitHub monad returning an Either within
--   the IO data type using the provided token
runGitHub' :: GitHub a -> BearerToken -> IO (Either GitHubReturnStatus a)
#if MIN_VERSION_errors(2,0,0)
runGitHub' comp token = evalStateT (runReaderT (runExceptT comp) token) defPagination
#else
runGitHub' comp token = evalStateT (runReaderT (runEitherT comp) token) defPagination
#endif

-- | Executes a computation built within the GitHub monad returning an Either within
--   the IO data type. Reads an API token from an environment variable named GITHUB_TOKEN
runGitHub :: GitHub a -> IO (Either GitHubReturnStatus a)
runGitHub comp = do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  case maybeToken of
    Just acquiredToken -> runGitHub' comp (BearerToken $ T.pack acquiredToken)
    Nothing            -> fail "Couldn't find GITHUB_TOKEN in environment"
