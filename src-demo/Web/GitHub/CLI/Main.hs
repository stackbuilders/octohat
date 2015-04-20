module Main where

import Web.GitHub.CLI.Options
import Web.GitHub.CLI.Actions

import Options.Applicative
import Network.Octohat.Types (OrganizationName(..), TeamName(..))
import qualified Data.Text as T (pack)

accessBotCLI :: TeamOptions -> IO ()
accessBotCLI (TeamOptions (ListTeams nameOfOrg)) =
  findTeamsInOrganization (OrganizationName $ T.pack nameOfOrg)
accessBotCLI (TeamOptions (ListMembers nameOfOrg nameOfTeam)) =
  findMembersInTeam (OrganizationName $ T.pack nameOfOrg) (TeamName $ T.pack nameOfTeam)
accessBotCLI (TeamOptions (AddToTeam nameOfOrg nameOfTeam nameOfUser)) =
  addUserToTeamInOrganization nameOfUser
                              (OrganizationName $ T.pack nameOfOrg)
                              (TeamName $ T.pack nameOfTeam)
accessBotCLI (TeamOptions (DeleteFromTeam nameOfOrg nameOfTeam nameOfUser)) =
  deleteUserFromTeamInOrganization nameOfUser
                                   (OrganizationName $ T.pack nameOfOrg)
                                   (TeamName $ T.pack nameOfTeam)

main :: IO ()
main = execParser argumentsParser >>= accessBotCLI

argumentsParser :: ParserInfo TeamOptions
argumentsParser = info (helper <*> teamOptions)
                    (fullDesc
                    <> progDesc "GitHub client to manage teams. Please specify your token as GITHUB_TOKEN"
                    <> header "Some options"
                    )
