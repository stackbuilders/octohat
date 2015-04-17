module Web.GitHub.CLI.Messages
  (commandMessage) where

import Network.Octohat.Types

commandMessage :: GitHubReturnStatus -> String
commandMessage NotFound               = "Organization or Team not found"
commandMessage NotAllowed             = "Not authorized"
commandMessage RequiresAuthentication = "Credentials must be configured to use this function"
commandMessage _                      = "Internal application error"
