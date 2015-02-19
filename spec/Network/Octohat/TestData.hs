{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.TestData ( loadTestOrganizationName
                                , loadOwnerTeam
                                , loadTestAccountOne
                                , loadTestAccountTwo
                                , loadTestAccountThree
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
    accountThree :: T.Text
  }

readEnv :: [(String, String)] -> Maybe TestEnvironment
readEnv environment =
  TestEnvironment <$> lookup "SANDBOX_ORGANIZATION" env
                  <*> lookup "TEST_ACCOUNT_ONE" env
                  <*> lookup "TEST_ACCOUNT_TWO" env
                  <*> lookup "TEST_ACCOUNT_THREE" env
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

publicKeyHostnameFixture :: T.Text
publicKeyHostnameFixture = "octohat@stackbuilders"

fingerprintFixture :: T.Text
fingerprintFixture = "9f:88:05:ab:2a:ac:79:5f:21:26:0c:56:28:71:fa:53"

publicKeyFixture :: T.Text
publicKeyFixture = "AAAAB3NzaC1yc2EAAAADAQABAAABAQDEOF9HByAB15X65dOi7rJGLS42rsvcCVxOXHD55CZe8zLdigUEuPPhN0rVaXxa936TYx1MpwR+oI741CYlinIvxJGx++/iXDx1JwxHjC0VKNRgWevn5TVOoa0fXgj9whCVK0savBIgizF3DG19J+VGW58ooMM86PpYLY/Usq3tnI3w9dr96moQRMUu6rSy8HyMhBuzU4LZ9D4sjEnuAneNQZ6HbNQtqkzTb9f7CdBDiC55PfLx1ilBVUHk7piU7GuesgVwddIa8J7J8Ljq2d/dMbmJrUFzHBlIEho7bzl1Mmh6JfWrGUwM82/eA6RAZNyG6yLM0crMcayAnsbvnm41"

fullPublicKeyFixture :: T.Text
fullPublicKeyFixture = T.concat ["ssh-rsa ", publicKeyFixture, " ", publicKeyHostnameFixture]
