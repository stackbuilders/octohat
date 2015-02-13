-- | Convenience functions for some common operations with teams. Execute the result of these
--   functions using 'runGitHub' or 'runGitHub''

module Network.Octohat ( addUserToTeam
                       , membersOfTeamInOrganization
                       , keysOfTeamInOrganization
                       , teamForTeamNameInOrg) where

import Control.Error.Safe (tryHead)
import Control.Monad (liftM)
import qualified Data.Text as T

import Network.Octohat.Keys
import Network.Octohat.Members
import Network.Octohat.Types


-- | Gets all the members of the organization
membersOfTeamInOrganization :: T.Text -- ^ GitHub organization name
                            -> T.Text -- ^ GitHub team name
                            -> GitHub [Member]
membersOfTeamInOrganization nameOfOrg nameOfTeam = teamId `liftM` teamForTeamNameInOrg nameOfOrg nameOfTeam >>= membersForTeam

-- | Adds a user with @nameOfUser@ to the team named @nameOfTeam@ within the organization named `nameOfOrg`
addUserToTeam :: T.Text -- ^ GitHub username
              -> T.Text -- ^ GitHub organization name
              -> T.Text -- ^ GitHub team name
              -> GitHub StatusInTeam
addUserToTeam nameOfUser nameOfOrg nameOfTeam = teamId `liftM` teamForTeamNameInOrg nameOfOrg nameOfTeam >>= addMemberToTeam nameOfUser

-- | Retrieves a list of members in a given team within an organization together with their public keys
keysOfTeamInOrganization :: T.Text -- ^ GitHub organization name
                         -> T.Text -- ^ GitHub team name
                         -> GitHub [MemberWithKey]
keysOfTeamInOrganization nameOfOrg nameOfTeam = do
  members <- membersOfTeamInOrganization nameOfOrg nameOfTeam
  pubKeys <- mapM keysForMember members
  let memberFingerprints = publicKeySetToFingerprints pubKeys
  return $ makeMembersWithKey members pubKeys memberFingerprints

teamForTeamNameInOrg :: T.Text -- ^ Organization name
                     -> T.Text -- ^ Team name
                     -> GitHub Team
teamForTeamNameInOrg nameOfOrg nameOfTeam = do
  teams <- teamsForOrganization nameOfOrg
  tryHead NotFound (teamsWithName nameOfTeam teams)

teamsWithName :: T.Text -> [Team] -> [Team]
teamsWithName nameOfTeam = filter (hasName nameOfTeam)

hasName :: T.Text -> Team -> Bool
hasName name team = name == teamName team
keysForMember :: Member -> GitHub [PublicKey]
keysForMember = publicKeysForUser . memberLogin

makeMembersWithKey :: [Member] -> [[PublicKey]] -> [[PublicKeyFingerprint]] -> [MemberWithKey]
makeMembersWithKey = zipWith3 MemberWithKey

publicKeySetToFingerprints :: [[PublicKey]] -> [[PublicKeyFingerprint]]
publicKeySetToFingerprints = (map.map) fingerprintFor


