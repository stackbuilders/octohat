{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.MembersSpec where

import Test.Hspec

import Network.Octohat.Members
import Network.Octohat.Types
import Network.Octohat.Keys
import Network.Octohat.TestData ( loadTestOrganizationName
                                , loadTestAccountOne
                                , loadTestAccountTwo
                                , fingerprintFixture
                                , loadTestAccountThree
                                , loadOwnerTeam
                                , publicKeyHostnameFixture
                                , publicKeyFixture
                                , fingerprintFixture)
import Network.Octohat.TestUtil (setupToken, removeTeams)


spec :: Spec
spec = after removeTeams $ before setupToken $ do
  describe "add teams" $ do
    it "should add a team to an organization and then delete it" $ do
      testOrganization <- loadTestOrganizationName
      Right ownerTeam  <- runGitHub loadOwnerTeam

      Right newTeam           <- runGitHub $ addTeamToOrganization "A new team" "A description" testOrganization
      Right teamsInOctohat    <- runGitHub $ teamsForOrganization testOrganization
      teamsInOctohat `shouldBe` [ownerTeam, newTeam]
      Right deleteResult      <- runGitHub $ deleteTeamFromOrganization (teamId newTeam)
      Right newTeamsInOctohat <- runGitHub $ teamsForOrganization testOrganization
      deleteResult `shouldBe` Deleted
      newTeamsInOctohat `shouldBe` [ownerTeam]

    it "should add a member to a new team and then delete it" $ do
      testOrganization     <- loadTestOrganizationName
      Right testAccountOne <- runGitHub loadTestAccountOne

      Right newTeam <- runGitHub $ addTeamToOrganization "A new team" "A description" testOrganization
      let idForThisTeam = teamId newTeam
      Right addingStatus <- runGitHub $ addMemberToTeam (memberLogin testAccountOne) idForThisTeam
      addingStatus `shouldBe` Active
      Right membersInThisTeam <- runGitHub $ membersForTeam idForThisTeam
      membersInThisTeam `shouldBe` [testAccountOne]

    it "should get the public keys for a user" $ do
      -- This assumes the token belongs to testAccountOne
      Right testAccountOne <- runGitHub loadTestAccountOne
      _ <- runGitHub $ addPublicKey publicKeyFixture publicKeyHostnameFixture

      Right pubKey <- runGitHub $ publicKeysForUser (memberLogin testAccountOne)
      fmap (publicKeyFingerprint . fingerprintFor) pubKey `shouldBe` [fingerprintFixture]

    it "should delete a member from a team" $ do
      testOrganization     <- loadTestOrganizationName
      Right testAccountOne <- runGitHub loadTestAccountOne

      Right newTeam    <- runGitHub $ addTeamToOrganization "Testing team" "Desc" testOrganization
      Right addStatus  <- runGitHub $ addMemberToTeam (memberLogin testAccountOne) (teamId newTeam)
      addStatus `shouldBe` Active

      Right membersInNewTeam <- runGitHub $ membersForTeam (teamId newTeam)
      [memberLogin $ head membersInNewTeam] `shouldBe` [memberLogin testAccountOne]

      Right deleteStatus <- runGitHub $ deleteMemberFromTeam (memberLogin testAccountOne) (teamId newTeam)
      [deleteStatus] `shouldBe` [Deleted]

      Right membersInNewTeamNow <- runGitHub $ membersForTeam (teamId newTeam)
      membersInNewTeamNow `shouldBe` []

    it "should list all members in the organization" $ do
      Right testAccountOne   <- runGitHub loadTestAccountOne
      Right testAccountTwo   <- runGitHub loadTestAccountTwo
      Right testAccountThree <- runGitHub loadTestAccountThree
      testOrganization       <- loadTestOrganizationName

      Right members <- runGitHub $ membersForOrganization testOrganization
      members `shouldMatchList` [testAccountOne, testAccountTwo, testAccountThree]
