{-# LANGUAGE OverloadedStrings #-}

-- | Execute the result of these functions using 'runGitHub' or 'runGitHub''

module Network.Octohat.Members
  ( membersForOrganization
  , teamsForOrganization
  , membersForTeam
  , reposForTeam
  , addMemberToTeam
  , addRepoToTeam
  , deleteMemberFromTeam
  , deleteTeamFromOrganization
  , publicKeysForUser
  , addTeamToOrganization
  , organizations
  , userForUsername
  , repoForReponame 
  , addPublicKey
  , resetPage
  ) where

import Network.Octohat.Internal
import Network.Octohat.Types

import qualified Data.Text as T
import Data.Monoid ((<>))

-- | Takes a new team name, the description of a team and the organization where to create the team
--   and creates a new team. Regular GitHub authorization/authentication applies.
addTeamToOrganization :: TeamName -- ^ Name of new team
                      -> T.Text  -- ^ Description of new team
                      -> TeamPermission -- ^ Permission setting for team (push, pull, or admin)
                      -> OrganizationName -- ^ Organization name where the team will be created
                      -> GitHub Team
addTeamToOrganization (TeamName nameOfNewTeam) descOfTeam teamPerm (OrganizationName org) =
  postRequestTo (composeEndpoint ["orgs", org, "teams"]) (TeamCreateRequest nameOfNewTeam descOfTeam teamPerm)

-- | Deletes a team from an organization using its team ID.
deleteTeamFromOrganization :: Integer          -- ^ ID of Team to delete
                           -> GitHub DidDelete
deleteTeamFromOrganization idOfTeam = deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam])

-- | Returns a list of members of an organization with the given name.
membersForOrganization :: OrganizationName -- ^ The organization name
                       -> GitHub [Member]
membersForOrganization (OrganizationName nameOfOrg) = getRequestPaginatedTo (composeEndpoint ["orgs", nameOfOrg, "members"])

-- | Returns a list of members of a team with the given team ID.
membersForTeam :: Integer         -- ^ The team ID
               -> GitHub [Member]
membersForTeam idOfTeam = getRequestPaginatedTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "members"])

-- | Returns a list of repos of a team with the given team ID.
reposForTeam :: Integer         -- ^ The team ID
              -> GitHub [Repo]
reposForTeam idOfTeam = getRequestPaginatedTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "repos"])

-- | Returns a list of teams for the organization with the given name
teamsForOrganization :: OrganizationName -- ^ The organization name
                     -> GitHub [Team]
teamsForOrganization (OrganizationName nameOfOrg) = getRequestPaginatedTo (composeEndpoint ["orgs", nameOfOrg, "teams"])

-- | Returns a list of all organizations for the user
organizations :: GitHub [Organization]
organizations = getRequestPaginatedTo (composeEndpoint ["user", "orgs"])

-- | Adds a member to a team, might invite or add the member. Refer to 'StatusInTeam'
addMemberToTeam :: T.Text               -- ^ The GitHub username to add to a team
                -> Integer              -- ^ The Team ID
                -> GitHub StatusInTeam
addMemberToTeam nameOfUser idOfTeam =
  putRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Adds a repo to a team, might invite or add the member. Refer to 'StatusInTeam'
addRepoToTeam :: OrganizationName     -- ^ The GitHub organization name 
              -> T.Text               -- ^ The GitHub repo name
              -> Integer              -- ^ The Team ID
              -> GitHub StatusInTeam
addRepoToTeam (OrganizationName nameOfOrg) nameOfRepo idOfTeam =
  putRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "repos", nameOfOrg, nameOfRepo])

-- | Deletes a member with the given name from a team with the given ID. Might or might not delete
deleteMemberFromTeam :: T.Text            -- ^ GitHub username
                     -> Integer           -- ^ GitHub team ID
                     -> GitHub DidDelete
deleteMemberFromTeam nameOfUser idOfTeam =
  deleteRequestTo (composeEndpoint ["teams", T.pack $ show idOfTeam, "memberships", nameOfUser])

-- | Returns the public keys of the user with the given name
publicKeysForUser :: T.Text             -- ^ GitHub username
                  -> GitHub [PublicKey]
publicKeysForUser nameOfUser = getRequestPaginatedTo (composeEndpoint ["users", nameOfUser, "keys"])

-- | Finds a user ID given their username
userForUsername :: T.Text        -- ^ GitHub username
                -> GitHub Member
userForUsername username = getRequestTo (composeEndpoint ["users", username])

-- | Finds a repo ID given their reponame
repoForReponame :: T.Text        -- ^ GitHub org
                -> T.Text        -- ^ GitHub repo
                -> GitHub Repo
repoForReponame org repo = getRequestTo (composeEndpoint ["repos", org, repo])

-- | Add a key for the currently authenticated user
addPublicKey :: T.Text -- ^ Base64 RSA Key (ssh-rsa AA..)
             -> T.Text -- ^ Key title, e.g @octocat@stackbuilders@
             -> GitHub PublicKey
addPublicKey newKey newTitle =
  postRequestTo (composeEndpoint ["user", "keys"]) (AddPublicKeyRequest ("ssh-rsa " <> newKey) newTitle)
