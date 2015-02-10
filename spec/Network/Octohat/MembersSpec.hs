{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.MembersSpec where

import Data.List (sort)
import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.Text as T

import Network.Octohat.Members
import Network.Octohat.Types
import Network.Octohat.Keys
import Network.Octohat.TestData (testOrganization, testAccount, ownerTeam, testAccountFingerprint)
import Network.Octohat.TestUtil (removeAllTeams)


spec :: Spec
spec = around_ removeAllTeams $
  describe "add teams" $ do
    it "should add a team to an organization and then delete it" $ do
      Right newTeam           <- runGitHub $ addTeamToOrganization "A new team" "A description" testOrganization
      Right teamsInOctohat    <- runGitHub $ teamsForOrganization testOrganization
      teamsInOctohat `shouldBe` [ownerTeam, newTeam]
      Right deleteResult      <- runGitHub $ deleteTeamFromOrganization (teamId newTeam)
      Right newTeamsInOctohat <- runGitHub $ teamsForOrganization testOrganization
      deleteResult `shouldBe` Deleted
      newTeamsInOctohat `shouldBe` [ownerTeam]

    it "should add a member to a new team and then delete it" $ do
      Right newTeam <- runGitHub $ addTeamToOrganization "A new team" "A description" testOrganization
      let idForThisTeam = teamId newTeam
      Right addingStatus <- runGitHub $ addMemberToTeam "jsantos-testaccount" idForThisTeam
      addingStatus `shouldBe` Active
      Right membersInThisTeam <- runGitHub $ membersForTeam idForThisTeam
      membersInThisTeam `shouldBe` [testAccount]

    it "should get the public keys for a user" $ do
      Right pubKey <- runGitHub $ publicKeysForUser "jsantos-testaccount"
      (fingerprintFor `fmap` pubKey) `shouldBe` [testAccountFingerprint]

    it "should delete a member from a team" $ do
      Right newTeam    <- runGitHub $ addTeamToOrganization "Testing team" "Desc" testOrganization
      Right addStatus  <- runGitHub $ addMemberToTeam "jsantos-testaccount"  (teamId newTeam)
      addStatus `shouldBe` Active

      Right membersInNewTeam <- runGitHub $ membersForTeam (teamId newTeam)
      [(memberLogin $ head membersInNewTeam)] `shouldBe` ["jsantos-testaccount"]

      Right deleteStatus <- runGitHub $ deleteMemberFromTeam "jsantos-testaccount" (teamId newTeam)
      [deleteStatus] `shouldBe` [Deleted]

      Right membersInNewTeamNow <- runGitHub $ membersForTeam (teamId newTeam)
      membersInNewTeamNow `shouldBe` []

    it "should list all members in the organization" $ do
      Right members <- runGitHub $ membersForOrganization testOrganization
      map memberLogin members `shouldMatchList` ["jsantos-testaccount", "jsantos-testaccount2", "jsantos17", "testUserForGithub"]
