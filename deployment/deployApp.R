library(tidyverse)

prodOpts <- list(
  Dev = 'ShweshweDev',
  QA = 'ShweshweQA'
)

deployApp <- function(prodOpt) {
  stopifnot(prodOpt %in% as.character(unname(prodOpts)))
  if (!file.exists("deployment/secrets.R")) {
    stop("You need to create a deployment/secrets.R file with `secrets = list(account=...,token=..., secret=...)`.")
  }
  source("deployment/secrets.R")
  appName <- prodOpt
  
  prodOpts <- list(
    Dev = 'ShweshweDev',
    QA = 'ShweshweQA'
  )
  
  rsconnect::setAccountInfo(name=secrets$account, secrets$token, secrets$secret)
  rsconnect::deployApp(account=secrets$account,
                       appName = appName,
                       forceUpdate = T)
}

deployAppDev <- function() {
  rstudioapi::jobRunScript(path = "deployment/deployDev.R",
                           name = "deployAppDev",
                           workingDir = here::here(),
                           importEnv = FALSE,
                           exportEnv = FALSE)
}

deployAppQA <- function() {
  rstudioapi::jobRunScript(path = "deployment/deployQA.R",
                           name = "deployAppQA",
                           workingDir = here::here(),
                           importEnv = FALSE,
                           exportEnv = FALSE)
}
