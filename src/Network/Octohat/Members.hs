{-# LANGUAGE OverloadedStrings #-}

-- | Execute the result of these functions using 'runGitHub' or 'runGitHub''

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

-- | Takes a new team name, the description of a team and the organization where to create the team
--   and creates a new team. Regular GitHub authorization/authentication applies.
addTeamToOrganization :: T.Text -> T.Text -> T.Text -> GitHub Team
addTeamToOrganization nameOfNewTeam descOfTeam orgName =
  postRequestTo (composeEndpoint ["orgs", orgName, "teams"]) (TeamCreateRequest nameOfNewTeam descOfTeam)

-- | Deletes a team from an organization using its team ID.
deleteTeamFromOrganization :: Integer -> GitHub DidDelete
deleteTeamFromOrganization idOfTeam = deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam])

-- | Returns a list of members of an organization with the given name.
membersForOrganization :: T.Text -> GitHub [Member]
membersForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "members"])

-- | Returns a list of members of a team with the given team ID.
membersForTeam :: Integer -> GitHub [Member]
membersForTeam idOfTeam = getRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "members"])

-- | Returns a list of teams for the organization with the given name
teamsForOrganization :: T.Text -> GitHub [Team]
teamsForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "teams"])

-- | Adds a member to a team, might invite or add the member. Refer to 'StatusInTeam'
addMemberToTeam :: T.Text -> Integer -> GitHub StatusInTeam
addMemberToTeam nameOfUser idOfTeam =
  putRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Deletes a member with the given name from a team with the given ID. Might or might not delete
deleteMemberFromTeam :: T.Text -> Integer -> GitHub DidDelete
deleteMemberFromTeam nameOfUser idOfTeam =
  deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Returns the public keys of the user with name @nameOfUser@
publicKeysForUser :: T.Text -> GitHub [PublicKey]
publicKeysForUser nameOfUser = getRequestTo (composeEndpoint ["users", nameOfUser, "keys"])
