# Creates new package-wide environment
if (!exists("RCandlesEnv")) {
  RCandlesEnv <- new.env(parent = .GlobalEnv)
}
