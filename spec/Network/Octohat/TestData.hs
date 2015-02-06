{-# LANGUAGE OverloadedStrings #-}

module Network.Octohat.TestData (testOrganization
                                , ownerTeam
                                , testAccountFingerprint
                                , testAccount
                                , testAccount2
                                , keysAndMembersOnNewTeam) where

import Network.Octohat.Types
import qualified Data.Text as T


-- WARNING Do not use a real organization here. The tests clean after themselves and delete all the teams
-- within the organization specified here.
testOrganization :: T.Text
testOrganization = "octohat-organization"

ownerTeam :: Team
ownerTeam = Team 1284248 "Owners" Nothing

testAccountFingerprint :: PublicKeyFingerprint
testAccountFingerprint =
  PublicKeyFingerprint {fingerprintId = 10865212
                       , publicKeyFingerprint = "b7:7d:f5:4e:d6:c3:32:fd:13:ed:a5:1a:13:c2:32:11"}

testAccountPublicKey :: PublicKey
testAccountPublicKey = PublicKey {publicKeyId = 10865212
                                 , publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCwXamXSk3zMNl/+F6Y6qotMhTTfUiYxDnuDi0+YMeg59CwW2GC9z2361WOqvR4WkJPjgpJ1JFuNJNJAIsFUS5fQBciO3EIXKaNSyYV4roivSjh6tTMsdkYZ8ASc5zmr9t3m/8d7/UdnG2PqiQOB9fml/5OWr1o1oNnFPn1CjzOod6xaBs0jr+TyXKBMlnQ6avPZgBu9150FfN/T9aXsGJskaxmMqbKkGLUUVxdaISoi1Ko93DWLLSyl63IjZZGIRIpv1MhjxtcnW0lEUwuP+GVRTL2WJygmMKAFGj4VP+KW8cuSxxpil7hgsRfSDkz626DW/Afi4uPqbRNHytR/a1T"
                                 }

testAccount :: Member
testAccount = Member "jsantos-testaccount" 10925231

testAccount2 :: Member
testAccount2 = Member "testUserForGithub" 10882308

--A team with testAccount and testAccount2 on it
keysAndMembersOnNewTeam :: [MemberWithKey]
keysAndMembersOnNewTeam =  [MemberWithKey {member = testAccount, memberKey = [testAccountPublicKey], memberKeyFingerprint = [testAccountFingerprint]}
                           , MemberWithKey {member = testAccount2, memberKey = [], memberKeyFingerprint = []}]
