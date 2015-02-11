# octohat

A well tested, GitHub API client using `wreq` as a backend

The project uses Stackage to maintain build stability

## Currently supported endpoints

### Members

`module Network.Octohat.Members`

* Add teams to an organization
  `addTeamToOrganization`

* Delete teams from an organization
  `deleteTeamFromOrganization`

* List all members from an organization
  `membersForOrganization`

* List all members from a team, using the team id
  `membersForTeam`

* List all teams from an organization
  `teamsForOrganization`

* Add members to a team
  `addMemberToTeam`

* Delete members from a team
  `deleteMemberFromTeam`

* List Public Keys for a user
  `publicKeysForUser`

## Instructions

To install:
```
cabal sandbox init
cabal install --only-dep --enable-test -jN
```

where N = \<the number of cores in your machine\>

To build:

```
cabal build
```

Then run the test suite:

```
cabal test
```
