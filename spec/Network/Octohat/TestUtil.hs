{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.TestUtil (findUserId, setupToken, removeTeams) where


import Network.Octohat.Members
import Network.Octohat.Types
import Network.Octohat.TestData

import Control.Monad.IO.Class (liftIO)
import Configuration.Dotenv (loadFile)
import qualified Data.Text as T

findUserId :: T.Text -> GitHub Integer
findUserId username = memberId `fmap` userForUsername username

deleteAllTeams :: GitHub [DidDelete]
deleteAllTeams = do
  testOrganization <- liftIO loadTestOrganizationName
  allTeams <- teamsForOrganization testOrganization
  let teamsToDelete = map teamId $ filter (\t -> teamName t /= "Owners") allTeams
  mapM deleteTeamFromOrganization teamsToDelete

setupToken :: IO ()
setupToken = do
  loadFile False ".env"
  result <- runGitHub deleteAllTeams
  case result of
    Left  _ -> fail "Clean-up failed"
    Right _ -> return ()

removeTeams :: () -> IO ()
removeTeams _ = do
  result <- runGitHub deleteAllTeams
  case result of
    Left  _ -> fail "Clean-up failed"
    Right _ -> return ()
