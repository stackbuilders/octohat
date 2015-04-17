module Web.GitHub.CLI.Options ( TeamOptions(..)
                              , TeamCommand(..)
                              , teamOptions) where

import Options.Applicative

type OrganizationName = String
type Username = String
type TeamName = String

data TeamCommand = ListTeams OrganizationName
                 | ListMembers OrganizationName TeamName
                 | AddToTeam OrganizationName TeamName Username
                 | DeleteFromTeam OrganizationName TeamName Username 

data TeamOptions = TeamOptions TeamCommand

teamOptions :: Parser TeamOptions
teamOptions = TeamOptions <$> parseTeamCommand

parseTeamCommand :: Parser TeamCommand
parseTeamCommand = subparser $
  command "list-teams"  (info listTeams      (progDesc "List teams in a organization" ))          <>
  command "members-in"  (info listMembers    (progDesc "List members in team and organization" )) <>
  command "add-to-team" (info addToTeam      (progDesc "Add users to a team"))                    <>
  command "delete-user" (info deleteFromTeam (progDesc "Delete a user from a team"))

listTeams :: Parser TeamCommand
listTeams = ListTeams <$> argument str (metavar "<organization name>")

listMembers :: Parser TeamCommand
listMembers = ListMembers <$> argument str (metavar "<organization-name>")
                          <*> argument str (metavar "<team-name>")

addToTeam :: Parser TeamCommand
addToTeam = AddToTeam <$> argument str (metavar "<organization name>")
                      <*> argument str (metavar "<team name>")
                      <*> argument str (metavar "<github username>")

deleteFromTeam :: Parser TeamCommand
deleteFromTeam = DeleteFromTeam <$> argument str (metavar "<organization name>")
                                <*> argument str (metavar "<team name>")
                                <*> argument str (metavar "<github username>")
