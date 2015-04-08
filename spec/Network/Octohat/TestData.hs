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
fingerprintFixture = "7c:11:af:a0:b7:ad:e4:76:d8:e0:d0:86:e0:7f:2a:fa"

publicKeyFixture :: T.Text
publicKeyFixture = "AAAAB3NzaC1yc2EAAAADAQABAAABAQCerupTuHE7jYE32vV9iKCwSLW44xkUprczufOLjlXVFWHe82CRPQyczBqJYHDk2AFNYGBQILTcPWLEDaPROqGMZBhdO684HBleTIWz9QrwEgLNNsXtQ8LGknyAWx7rJzBRQGh4Qxwxpbs0x0W/CqWun5x9XSO/mxTT5I2uZrp4D11aoV+bCEpe/Em4LdqTHaDXPN48oJz0sbmx4/dK19lowTMDrS/4zUTQKbdNaZpdJPYpPNt8vnf0MYiABqCSVynL70g2NM7suzO99DwwiDPmoYsIDpkPxt6xARdX1bJX0iNfv0n+BNuUZ/81ysvBj7IiZ+/bz49NOXRmKp+UbDh1"

fullPublicKeyFixture :: T.Text
fullPublicKeyFixture = T.concat ["ssh-rsa ", publicKeyFixture, " ", publicKeyHostnameFixture]
