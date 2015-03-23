{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.TestData ( loadTestOrganizationName
                                , loadOwnerTeam
                                , loadTestAccountOne
                                , loadTestAccountTwo
                                , loadTestAccountThree
                                , loadTestAccountFour
                                , publicKeyFixture
                                , publicKeyHostnameFixture
                                , fingerprintFixture
                                , fullPublicKeyFixture
                                ) where

import Network.Octohat.Types
import Network.Octohat (teamForTeamNameInOrg)
import Network.Octohat.Members (userForUsername)

import Control.Monad.IO.Class (liftIO)
import Control.Arrow (second)
import System.Environment.Compat (getEnvironment)
import Configuration.Dotenv (loadFile)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T

data TestEnvironment =
  TestEnvironment {
    organization :: T.Text,
    accountOne   :: T.Text,
    accountTwo   :: T.Text,
    accountThree :: T.Text,
    accountFour  :: T.Text
  }

readEnv :: [(String, String)] -> Maybe TestEnvironment
readEnv environment =
  TestEnvironment <$> lookup "SANDBOX_ORGANIZATION" env
                  <*> lookup "TEST_ACCOUNT_ONE" env
                  <*> lookup "TEST_ACCOUNT_TWO" env
                  <*> lookup "TEST_ACCOUNT_THREE" env
                  <*> lookup "TEST_ACCOUNT_FOUR" env
    where env = map (second T.pack) environment

loadEnv :: IO TestEnvironment
loadEnv = do
  loadFile False ".github-sandbox"
  env <- getEnvironment
  case readEnv env of
    Just res -> return res
    Nothing  -> fail "Environment variables not set correctly, please read README.md"

loadTestOrganizationName :: IO T.Text
loadTestOrganizationName = organization `fmap` loadEnv

loadOwnerTeam :: GitHub Team
loadOwnerTeam = liftIO (OrganizationName `fmap` (organization `fmap` loadEnv)) >>= flip teamForTeamNameInOrg (TeamName "Owners")

loadTestAccountOne :: GitHub Member
loadTestAccountOne = liftIO (accountOne `fmap` loadEnv) >>= userForUsername

loadTestAccountTwo :: GitHub Member
loadTestAccountTwo = liftIO (accountTwo `fmap` loadEnv) >>= userForUsername

loadTestAccountThree :: GitHub Member
loadTestAccountThree = liftIO (accountThree `fmap` loadEnv) >>= userForUsername

loadTestAccountFour :: GitHub Member
loadTestAccountFour = liftIO (accountFour `fmap` loadEnv) >>= userForUsername

publicKeyHostnameFixture :: T.Text
publicKeyHostnameFixture = "octohat@stackbuilders"

fingerprintFixture :: T.Text
fingerprintFixture = "42:59:20:02:6f:df:b4:4a:1c:0e:fd:1b:86:58:f6:06"

publicKeyFixture :: T.Text
publicKeyFixture = "AAAAB3NzaC1yc2EAAAADAQABAAABAQC1Dopc3yxLWlzJwFqSoj0nAzRCU93R5DwNlogtRr/7NsnUVf443wl/vpRDRNscR0dV/VeNWYCqiZA0wGrXiVJ7HYi9XaWtHrUutLqrLe47aFFvAIdp15+RHkM0sXr963Kb9XMkmqswyXJ2TaZ0cgZfMNgl1ND248Y8fMDBx8elHwdZvyG2onG5aSVtOuKB4dWnmIb+uSQCN1K2kLYwHvQOjmqCiZ2XOP9u+ScphVdp6x4uAczH67CCRSUhI6U2fxSNf6YaDXyCWcqxj1agHUKdskb5rzxPaz5XZ2BgQscjoVo93M338HLmkyvbuP4yl2X6ZdLfE5mk2ZFfWQogxLGd"

fullPublicKeyFixture :: T.Text
fullPublicKeyFixture = T.concat ["ssh-rsa ", publicKeyFixture, " ", publicKeyHostnameFixture]
