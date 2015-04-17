module Web.GitHub.CLI.Actions
  ( findTeamsInOrganization
  , addUserToTeamInOrganization
  , deleteUserFromTeamInOrganization
  , findMembersInTeam) where

import Web.GitHub.CLI.Messages
import Network.Octohat.Members (teamsForOrganization, deleteMemberFromTeam)
import Network.Octohat
import Network.Octohat.Types
import qualified Data.Text as T
import Data.Yaml as Y
import Data.List (find)
import Data.ByteString.UTF8 (toString)
import System.IO (hPutStrLn, stderr)
import System.Exit (ExitCode(..), exitWith)

findTeamsInOrganization :: OrganizationName -> IO ()
findTeamsInOrganization nameOfOrg = do
  teamListing <- runGitHub $ teamsForOrganization nameOfOrg
  case teamListing of
    Left status -> errorHandler $ commandMessage status
    Right teams -> putStrLn (toString $ Y.encode teams)

addUserToTeamInOrganization :: String -> OrganizationName -> TeamName -> IO ()
addUserToTeamInOrganization nameOfUser nameOfOrg nameOfTeam = do
  addResult <- runGitHub $ addUserToTeam (T.pack nameOfUser) nameOfOrg nameOfTeam
  case addResult of
    Left status   -> errorHandler $ commandMessage status
    Right Pending -> putStrLn "User invited to join team"
    Right Active  -> putStrLn "User added to team"

deleteUserFromTeamInOrganization :: String -> OrganizationName -> TeamName -> IO ()
deleteUserFromTeamInOrganization nameOfUser nameOfOrg nameOfTeam = do
  teamListing <- runGitHub $ teamsForOrganization nameOfOrg
  case teamListing of
    Left status -> errorHandler $ commandMessage status
    Right []    -> errorHandler "No teams found on this organization"
    Right teams -> getTheTeamAndDeleteUser nameOfUser nameOfTeam teams

findMembersInTeam :: OrganizationName -> TeamName -> IO ()
findMembersInTeam nameOfOrg nameOfTeam = do
  memberListing <- runGitHub $ membersOfTeamInOrganization nameOfOrg nameOfTeam
  case memberListing of
    Left status   -> errorHandler $ commandMessage status
    Right members -> putStrLn (toString $ Y.encode members)

errorHandler :: String -> IO ()
errorHandler message = hPutStrLn stderr message >> exitWith (ExitFailure 1)

getTheTeamAndDeleteUser :: String -> TeamName -> [Team] -> IO ()
getTheTeamAndDeleteUser nameOfUser nameOfTeam teams = 
  case (getTeam teams) of
    Nothing   -> errorHandler "There's no such team in that organization"
    Just team -> do
      result <- runGitHub $ deleteMemberFromTeam (T.pack nameOfUser) (teamId team)
      case result of
        Left status      -> errorHandler $ commandMessage status
        Right NotDeleted -> errorHandler "The user could not be deleted from the team"
        Right Deleted    -> putStrLn "The user was succesfully deleted from the team"
  where getTeam = find (\t -> (teamName t) == (unTeamName nameOfTeam))
