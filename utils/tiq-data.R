## tiq-data.R
##
## Data loading/parsing utility functions

## Requires: Usage of `data.table` package
library(data.table)

## Path utility functions
tiq.data.enrichmentPath <- function() {
  msg = "Not Implemented"
  flog.error(msg)
  stop(msg)
}

tiq.data.populationPath <- function() {
  msg = "Not Implemented"
  flog.error(msg)
  stop(msg)
}

tiq.data.rawPath <- function() {
  msg = "Not Implemented"
  flog.error(msg)
  stop(msg)
}

tiq.data.loadPopulationData <- function(pop.group, pop.id, date = NULL) {

  ## Checking the population group
  if (missing(pop.group)) {
    msg = "tiq.loadPopulationData: unable to open Population Data - `pop.group` is missing"
    flog.error(msg)
    stop(msg)
  }

  ## Is there a date?

  msg = "Not Implemented"
  flog.error(msg)
  stop(msg)
}
