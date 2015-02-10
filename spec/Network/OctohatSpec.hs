{-# LANGUAGE OverloadedStrings #-}

module Network.OctohatSpec where

import Test.Hspec
import Test.Hspec.Expectations

import Network.Octohat.Members
import Network.Octohat.Keys
import Network.Octohat.Types
import Network.Octohat
import Network.Octohat.TestData (testOrganization, testAccount, testAccount2, ownerTeam, keysAndMembersOnNewTeam)
import Network.Octohat.TestUtil (removeAllTeams)

spec :: Spec
spec = around_ removeAllTeams $ do
  describe "keysOfTeamInOrganization" $ do
    it "should return the members and their keys of an specific team" $ do
      Right newTeam           <- runGitHub $ addTeamToOrganization "A new Team" "New description" testOrganization
      let idNewTeam = teamId newTeam
      Right teamsInOctohat    <- runGitHub $ teamsForOrganization testOrganization
      teamsInOctohat `shouldBe` [ownerTeam, newTeam]
      Right addingStatus1     <- runGitHub $ addMemberToTeam "jsantos-testaccount" idNewTeam
      addingStatus1 `shouldBe` Active
      Right addingStatus2     <- runGitHub $ addMemberToTeam "testUserForGithub" idNewTeam
      addingStatus2 `shouldBe` Active
      Right membersInThisTeam <- runGitHub $ membersForTeam idNewTeam
      membersInThisTeam `shouldMatchList` [testAccount, testAccount2]
      Right membersAndKeys    <- runGitHub $ keysOfTeamInOrganization testOrganization (teamName newTeam)
      membersAndKeys `shouldMatchList` keysAndMembersOnNewTeam

  describe "membersOfTeamInOrganization" $ do
    it "should return the members of a team on a organization, using organization and team names" $ do
      Right newTeam <- runGitHub $ addTeamToOrganization "A new Team" "New description" testOrganization
      let idNewTeam = teamId newTeam
      Right teamsInOctohat <- runGitHub $ teamsForOrganization testOrganization
      teamsInOctohat `shouldMatchList` [ownerTeam, newTeam]
      Right addingStatus <- runGitHub $ addMemberToTeam "jsantos-testaccount" idNewTeam
      addingStatus`shouldBe` Active
      Right membersInThisTeam <- runGitHub $ membersOfTeamInOrganization testOrganization (teamName newTeam)
      membersInThisTeam `shouldBe` [testAccount]
