# Octohat
[![Build Status](https://travis-ci.org/stackbuilders/octohat.svg?branch=master)](https://travis-ci.org/stackbuilders/octohat)
[![Hackage](https://img.shields.io/hackage/v/octohat.svg)](http://hackage.haskell.org/package/octohat)

A well tested, GitHub API client for Haskell using `wreq` as a backend.

The project uses Stackage to maintain build stability.

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

### To install:
```
cabal sandbox init
cabal install --only-dep --enable-test -jN
```

where N = \<the number of cores in your machine\>

### To build:

```
cabal build
```

### Then run the test suite:

**CAUTION: Use tokens of a test user, not your own account since the test suite clears state on Github before it runs (i.e., it will delete everything in your github account).**

You need to set some environment variables.

Set `SANDBOX_ORGANIZATION` to an organization you don't care about, since the tests will delete all the teams within that organization

Set `TEST_ACCOUNT_ONE` to a test account member of the Owners teams in `$SANDBOX_ORGANIZATION`

Set `TEST_ACCOUNT_TWO` to another test account member of the Owners teams in `$SANDBOX_ORGANIZATION`

Set `TEST_ACCOUNT_THREE` to yet another test account member of the Owners teams in `$SANDBOX_ORGANIZATION`

Set `GITHUB_TOKEN` to an API token from `$TEST_ACCOUNT_ONE`. The token scopes should include full admin privileges and the ability to write public keys.

You can these variables either directly or put them in `.GITHUB_SANDBOX`. Either way they must be set to run the tests. Finally run the tests with:

```
cabal test
```

### Demo

After you have built the binaries using `cabal build` you should have an executable file named `.dist/build/abc/abc`. This provides a rather basic interface to the GitHub API. The tool expects the environment variable `GITHUB_TOKEN` to be set to a GitHub token with permissions to modify an organization. Some of the operations supported: (each subcommand has its own `--help` page)

`dist/build/abc/abc --help`

```
Some options

Usage: abc COMMAND
  GitHub client to manage teams. Please specify your token as GITHUB_TOKEN

Available options:
  -h,--help                Show this help text

Available commands:
  list-teams               List teams in a organization
  members-in               List members in team and organization
  add-to-team              Add users to a team
  delete-user              Delete a user from a team
```

## License

MIT, see the LICENSE file.
