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
  , userForUsername
  , addPublicKey
  ) where

import Network.Octohat.Internal
import Network.Octohat.Types

import qualified Data.Text as T
import Data.Monoid ((<>))

-- | Takes a new team name, the description of a team and the organization where to create the team
--   and creates a new team. Regular GitHub authorization/authentication applies.
addTeamToOrganization :: T.Text  -- ^ Name of new team
                      -> T.Text  -- ^ Description of new team
                      -> T.Text  -- ^ Organization name where the team will be created
                      -> GitHub Team
addTeamToOrganization nameOfNewTeam descOfTeam orgName =
  postRequestTo (composeEndpoint ["orgs", orgName, "teams"]) (TeamCreateRequest nameOfNewTeam descOfTeam)

-- | Deletes a team from an organization using its team ID.
deleteTeamFromOrganization :: Integer          -- ^ ID of Team to delete
                           -> GitHub DidDelete
deleteTeamFromOrganization idOfTeam = deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam])

-- | Returns a list of members of an organization with the given name.
membersForOrganization :: T.Text          -- ^ The organization name
                       -> GitHub [Member]
membersForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "members"])

-- | Returns a list of members of a team with the given team ID.
membersForTeam :: Integer         -- ^ The team ID
               -> GitHub [Member]
membersForTeam idOfTeam = getRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "members"])

-- | Returns a list of teams for the organization with the given name
teamsForOrganization :: T.Text        -- ^ The organization name
                     -> GitHub [Team]
teamsForOrganization nameOfOrg = getRequestTo (composeEndpoint ["orgs", nameOfOrg, "teams"])

-- | Adds a member to a team, might invite or add the member. Refer to 'StatusInTeam'
addMemberToTeam :: T.Text               -- ^ The GitHub username to add to a team
                -> Integer              -- ^ The Team ID
                -> GitHub StatusInTeam
addMemberToTeam nameOfUser idOfTeam =
  putRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Deletes a member with the given name from a team with the given ID. Might or might not delete
deleteMemberFromTeam :: T.Text            -- ^ GitHub username
                     -> Integer           -- ^ GitHub team ID
                     -> GitHub DidDelete
deleteMemberFromTeam nameOfUser idOfTeam =
  deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Returns the public keys of the user with the given name
publicKeysForUser :: T.Text             -- ^ GitHub username
                  -> GitHub [PublicKey]
publicKeysForUser nameOfUser = getRequestTo (composeEndpoint ["users", nameOfUser, "keys"])

-- | Finds a user ID given their username
userForUsername :: T.Text        -- ^ GitHub username
                -> GitHub Member
userForUsername username = getRequestTo (composeEndpoint ["users", username])

-- | Add a key for the currently authenticated user
addPublicKey :: T.Text -- ^ Base64 RSA Key (ssh-rsa AA..)
             -> T.Text -- ^ Key title, e.g @octocat@stackbuilders@
             -> GitHub PublicKey
addPublicKey newKey newTitle =
  postRequestTo (composeEndpoint ["user", "keys"]) (AddPublicKeyRequest ("ssh-rsa " <> newKey) newTitle)
