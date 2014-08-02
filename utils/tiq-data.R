## tiq-data.R
##
## Data loading/parsing utility functions
##
## This module attemps to abstract the loading of TI feeds using the directory
## structure we propose for the initial release.

# Requires: Usage of `data.table` and `testthat` packages
library(data.table)
library(testthat)

################################################################################
## Helper path bulding and enumeration functions for the 'database'
################################################################################
# The root path is the local working directory of tiq-test scripts with an added
# "data" path. Change here if you want it to point to a different place.
.tiq.data.rootPath = file.path(getwd(), "data")

# tiq.data.getDirPath - returns 'character'
# Builds the path for the intel analysts sources and returns it.
#  - category: currently one of "raw", "enriched", "population"
#  - group: the intel group you want the path for
.tiq.data.getDirPath <- function(category, group) {
  path = file.path(.tiq.data.rootPath, category, group)
  if (!file.exists(path)) {
    msg = sprintf("tiq.data.getDirPath: path '%s' is invalid. Check 'category' and 'group' args.",
                  path)
    flog.error(msg)
    stop(msg)
  }

  flog.trace("tiq.data.getDirPath: returning path '%s'", path)
  return(path)
}

# tiq.data.getAvailableDates - returns 'character' vector
# Queries the data repository for the specific 'category' and 'group' of TI
# or population information and return the dates we have available for that
# group.
#  - category: currently one of "raw", "enriched", "population"
#  - group: the intel group you want the path for
.tiq.data.getAvailableDates <- function(category, group) {
  path = .tiq.data.getDirPath(category, group)

  # Only get the files that match our description for this "database"
  path.files = list.files(path)
  path.files = grep("^[0123456789]{8}[.]csv[.]gz$", path.files, value=T)

  dates = sub(".csv.gz", "", path.files, fixed=T)
  return(dates)
}

################################################################################
## TI and Population loading data functions
################################################################################
# tiq.data.loadTI - returns a 'data.table' with at least '.tiq.data.defaultTIFields'
# Fetches the data contained in the Threat Intelligence dataset contained in
# 'category' and 'group' on the 'date' specified.
#  - category: currently one of "raw" or "enriched"
#  - group: the intel group you want the path for
#  - date: a date string in the format "YYYYMMDD", or NULL if you want the
#          date the 'database' has available
.tiq.data.defaultTIFields = c("entity", "type", "direction", "source", "notes", "date")
tiq.data.loadTI <- function(category, group, date=NULL) {
  return(.tiq.data.loadData(category, group, date, valid.fields=.tiq.data.defaultTIFields))
}

# tiq.data.loadPopulation - returns a 'data.table' with 'pop.id' and 'totalIPs'
# Fetches the data contained in the Population dataset contained in
# 'pop.group', with the 'pop.id' as the key on the 'date' specified.
#  - pop.group: the intel group you want the path for
#  - pop.id: the key of the population dataset. If not present in the dataset
#            the function will return NULL.
#  - date: a date string in the format "YYYYMMDD", or NULL if you want the
#          date the 'database' has available
tiq.data.loadPopulation <- function(pop.group, pop.id, date = NULL) {
  pop.data = .tiq.data.loadData("population", pop.group, date, valid.fields=pop.id)
  if (!is.null(pop.data)) {
    pop.data[, totalIPs := as.numeric(totalIPs)]
    pop.data = list(pop.data)
    names(pop.data) <- pop.group
  }
  return(pop.data)
}

# .tiq.data.loadData - returns a 'data.table'
# Lower level function helper for loadTI and loadPopulation. Should not be used
# directly
#  - category: currently one of "raw", "enriched" or "population"
#  - group: the intel group you want the path for
#  - date: a date string in the format "YYYYMMDD", or NULL if you want the
#          date the 'database' has available
#  - valid.fields: the fields we should look for in the dataset to validate that
#                  it is of the correct type
.tiq.data.loadData <- function(category, group, date=NULL, valid.fields=character(0)) {

  test_that("tiq.data.loadData: unable to open data - 'category' or 'group' are invalid", {
    expect_that(class(category), equals("character"))
    expect_that(class(group), equals("character"))
  })

  # If we do not have a date, we get the latest one we have available
  # The dates are in "%Y%M%d" format, so they can be lexicographically ordered
  if (is.null(date)) {
    date = max(.tiq.data.getAvailableDates(category, group))
  }

  # Getting the data path
  ti.path = .tiq.data.getDirPath(category, group)
  ti.file = file.path(ti.path, paste0(date, ".csv.gz"))

  if (!file.exists(ti.file)) {
    flog.warn("tiq.data.loadTI: path '%s' is invalid. No data available on date '%s'.",
              ti.file, date)
    return(NULL)
  }

  # Getting and formatting the data
  ti.dt = as.data.table(read.csv(ti.file, header=T, na.strings="",
                                 stringsAsFactors=F, colClasses="character"))

  # Some sanity checking - is this a data.table and does it have all the fields
  # we recognize?
  if (!is.data.table(ti.dt) || !all(valid.fields %chin% names(ti.dt))) {
    flog.warn("tiq.data.loadData: Data loaded from path '%s' is invalid.",
              ti.file)
    return(NULL)
  }

  return(ti.dt)
}
