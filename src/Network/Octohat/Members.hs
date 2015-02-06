{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.Members
  ( membersForOrganization
  , membersForTeam
  , teamsForOrganization
  , addMemberToTeam
  , deleteMemberFromTeam
  , deleteTeamFromOrganization
  , publicKeysForUser
  , addTeamToOrganization
  ) where

import Network.Octohat.Internal
import Network.Octohat.Types

import qualified Data.Text as T

type TeamId = Integer
type Username = T.Text


addTeamToOrganization :: T.Text -> T.Text -> T.Text -> GitHub Team
addTeamToOrganization nameOfNewTeam descOfTeam orgName =
  postRequestTo (composeEndpoint ["orgs", orgName, "teams"]) (TeamCreateRequest nameOfNewTeam descOfTeam)

deleteTeamFromOrganization :: TeamId -> GitHub DidDelete
deleteTeamFromOrganization idOfTeam = deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam])

membersForOrganization :: T.Text -> GitHub [Member]
membersForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "members"])

membersForTeam :: TeamId -> GitHub [Member]
membersForTeam idOfTeam = getRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "members"])

teamsForOrganization :: T.Text -> GitHub [Team]
teamsForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "teams"])

addMemberToTeam :: T.Text -> TeamId -> GitHub StatusInTeam
addMemberToTeam nameOfUser idOfTeam =
  putRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

deleteMemberFromTeam :: T.Text -> TeamId -> GitHub DidDelete
deleteMemberFromTeam nameOfUser idOfTeam =
  deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

publicKeysForUser :: Username -> GitHub [PublicKey]
publicKeysForUser nameOfUser = getRequestTo (composeEndpoint ["users", nameOfUser, "keys"])
