{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.Types ( MemberType
                             , Member(..)
                             , MemberWithKey(..)
                             , Team(..)
                             , BearerToken(..)
                             , runGitHub
                             , StatusInTeam(..)
                             , EmptyBody(..)
                             , DidDelete(..)
                             , PublicKey(..)
                             , PublicKeyFingerprint(..)
                             , TeamCreateRequest(..)
                             , GitHubReturnStatus(..)
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

data MemberType = Developer | ProjectManager | Administrator deriving (Show, Eq)

data Member =
  Member { memberLogin :: T.Text
         , memberId    :: Integer
         } deriving (Show, Eq)

data Team =
  Team { teamId          :: Integer
       , teamName        :: T.Text
       , teamDescription :: Maybe T.Text
       } deriving (Show, Eq)

data TeamCreateRequest =
  TeamCreateRequest { newTeamName        :: T.Text
                    , newTeamDescription :: T.Text
                    } deriving (Show, Eq)

data MemberWithKey =
  MemberWithKey { member               :: Member
                , memberKey            :: [PublicKey]
                , memberKeyFingerprint :: [PublicKeyFingerprint]
                } deriving (Show, Eq)

data PublicKey =
  PublicKey { publicKeyId :: Integer
            , publicKey   :: T.Text
            } deriving (Show, Eq)

data PublicKeyFingerprint =
  PublicKeyFingerprint { fingerprintId        :: Integer
                       , publicKeyFingerprint :: T.Text
                       } deriving (Show, Eq)

data EmptyBody    = EmptyBody deriving (Show, Eq)
data StatusInTeam = Active  | Pending deriving (Show, Eq)
data DidDelete    = Deleted | NotDeleted deriving (Show, Eq)

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

data GitHubReturnStatus = InvalidJSON | ValidationFailed | InternalError | NotFound | AllOk | UnexpectedJSON String deriving (Show, Eq)

instance Putable EmptyBody where
  putPayload EmptyBody req = return $ req {requestBody = RequestBodyLBS ""}

instance Postable TeamCreateRequest where
  postPayload createRequest req = return $ req { requestBody = RequestBodyLBS (encode createRequest)}

newtype BearerToken = BearerToken { unBearerToken :: T.Text }

type GitHub = EitherT GitHubReturnStatus (ReaderT BearerToken IO)

runGitHub' :: GitHub a -> BearerToken -> IO (Either GitHubReturnStatus a)
runGitHub' comp = runReaderT (runEitherT comp)

runGitHub :: GitHub a -> IO (Either GitHubReturnStatus a)
runGitHub comp = do
  maybeToken <- lookupEnv "GITHUB_TOKEN"
  case maybeToken of
    Just acquiredToken -> runGitHub' comp (BearerToken $ T.pack acquiredToken)
    Nothing            -> fail "Couldn't find GITHUB_TOKEN in environment"
