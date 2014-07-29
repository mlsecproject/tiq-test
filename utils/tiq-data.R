## tiq-data.R
##
## Data loading/parsing utility functions

## Requires: Usage of `data.table` package
library(data.table)
library(testthat)

## The root path is the local working directory of tiq-test scripts with an added
## "data" path. Change here if you want it to point to a different place.
.tiq.data.rootPath = file.path(getwd(), "data")
.tiq.data.defaultTIFields = c("entity", "type", "direction", "source", "notes", "date")

## tiq.data.getDirPath
## Builds the path for the intel analysts sources.
##  - category: currently one of "raw", "enriched", "population"
##  - group: the intel group you want the path for
.tiq.data.getDirPath <- function(category, group) {
  path = file.path(.tiq.data.rootPath, category, group)
  if (!file.exists(path)) {
    msg = sprintf("tiq.data.getDirPath: path '%s' is invalid. Check 'category' and 'group' args.",
                  path)
    flog.error(msg)
    stop(msg)
  }

  return(path)
}

.tiq.data.getAvailableDates <- function(category, group) {
  path = .tiq.data.getDirPath(category, group)

  # Only get the files that match our description for this "database"
  path.files = list.files(path)
  path.files = grep("^[0123456789]{8}[.]csv[.]gz$", path.files, value=T)

  dates = sub(".csv.gz", "", path.files, fixed=T)
  return(dates)
}

tiq.data.loadPopulation <- function(pop.group, pop.id, date = NULL) {
  return(.tiq.data.loadData("population", pop.group, date, valid.fields=pop.id))
}

tiq.data.loadTI <- function(category, group, date=NULL) {
  return(.tiq.data.loadData(category, group, date, valid.fields=.tiq.data.defaultTIFields))
}

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

tiq.data.extractPopulationFromTI <- function(category, group, pop.id, date=NULL) {

  ti.dt = tiq.data.loadTI(category, group, date)
  if (is.null(ti.dt)) {
    msg = sprintf("tiq.data.extractPopulationFromTI: unable to locate TI for '%s', '%s', '%s'",
                  category, group, ifelse(is.null(date), "NULL", date))
    flog.error(msg)
    stop(msg)
  }

  if (missing(pop.id) || !all(pop.id %chin% names(ti.dt))) {
    msg = sprintf("tiq.data.extractPopulationFromTI: all pop.id fields '%s' must be present in TI fields '%s'",
                  paste(pop.id, collapse="', '"),
                  paste(names(ti.dt), collapse="', '"))
    flog.error(msg)
    stop(msg)
  }

  split.ti = split(ti.dt, ti.dt$source)

  aa = split.ti[[1]]
  pop.aa = aa[, list(totalIPs=.N), by=pop.id]
  setkeyv(pop.aa, pop.id)
}

if (F) {
category = "enriched"
group = "public_outbound"
pop.id = "country"
date=NULL
pop.group = "mmgeo"

}
