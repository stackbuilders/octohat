module Network.Octohat (membersOfTeamInOrganization, keysOfTeamInOrganization) where

import Control.Error.Safe (tryHead)
import qualified Data.Text as T

import Network.Octohat.Keys
import Network.Octohat.Members
import Network.Octohat.Types

type TeamName = T.Text
type OrgName  = T.Text

membersOfTeamInOrganization :: OrgName -> TeamName -> GitHub [Member]
membersOfTeamInOrganization nameOfOrg nameOfTeam = do
  teams     <- teamsForOrganization nameOfOrg
  foundTeam <- tryHead NotFound (teamsWithName nameOfTeam teams)
  membersForTeam (teamId foundTeam)

keysOfTeamInOrganization :: OrgName -> TeamName -> GitHub [MemberWithKey]
keysOfTeamInOrganization nameOfOrg nameOfTeam = do
  members <- membersOfTeamInOrganization nameOfOrg nameOfTeam
  pubKeys <- mapM keysForMember members
  let memberFingerprints = publicKeySetToFingerprints pubKeys
  return $ makeMembersWithKey members pubKeys memberFingerprints

keysForMember :: Member -> GitHub [PublicKey]
keysForMember = publicKeysForUser . memberLogin

makeMembersWithKey :: [Member] -> [[PublicKey]] -> [[PublicKeyFingerprint]] -> [MemberWithKey]
makeMembersWithKey = zipWith3 MemberWithKey

publicKeySetToFingerprints :: [[PublicKey]] -> [[PublicKeyFingerprint]]
publicKeySetToFingerprints = (map.map) fingerprintFor

teamsWithName :: TeamName -> [Team] -> [Team]
teamsWithName nameOfTeam = filter (hasName nameOfTeam)

hasName :: TeamName -> Team -> Bool
hasName name team = name == teamName team
