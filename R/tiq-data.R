## tiq-data.R
##
## Data loading/parsing utility functions
##
## This module attemps to abstract the loading of TI feeds using the directory
## structure we propose for the initial release.

# Helper path bulding and enumeration functions for the 'database' --------

#' Set data path
#'
#' @param newRootPath root data path
#' @export
setRootPath <- function(newRootPath) {
  flog.info("setRootPath: Setting path to '%s'", newRootPath)
  Sys.setenv(TIQ_DATA_ROOT_PATH=newRootPath)
}

#' Builds the path for the intel analysts sources and returns it.
#'
#' @param category currently one of "raw", "enriched", "population"
#' @param group the intel group you want the path for
#' @return 'character'
#' @export
getDirPath <- function(category, group) {
  path = file.path(Sys.getenv("TIQ_DATA_ROOT_PATH", unset = "./data"), category, group)
  if (!file.exists(path)) {
    msg = sprintf("getDirPath: path '%s' is invalid. Check 'category' and 'group' args.",
                  path)
    flog.error(msg)
    stop(msg)
  }

  flog.trace("getDirPath: returning path '%s'", path)
  return(path)
}

#' Queries the data repository for the specific 'category' and 'group' of TI
#' or population information and return the dates we have available for that
#' group.
#'
#' @param category currently one of "raw", "enriched", "population"
#' @param group the intel group you want the path for
#' @return 'character' vector
#' @export
getAvailableDates <- function(category, group) {
  path = getDirPath(category, group)

  # Only get the files that match our description for this "database"
  path.files = list.files(path)
  path.files = grep("^[[:digit:]]{8}[.]csv[.]gz$", path.files, value=T)

  dates = sub(".csv.gz", "", path.files, fixed=T)
  return(dates)
}

# getDateSequence - returns 'character' vector
# Given a 'start.date' and an 'end.date' both in character "YYYYMMDD" format
# creates a character vector of the sequence of dates between both
#  - start.date: the initial date of the sequence
#  - end.date: the final date of the sequence
getDateSequence <- function(start.date, end.date) {
  # We need to have at least 2 different sequantial days to greate the range
  if (end.date <= start.date) {
    msg = sprintf("getDateSequence: The end.date %s must be later than the start.date %s",
                  end.date, start.date)
    flog.error(msg)
    stop(msg)
  }

  date.range = sapply(
    seq(from=as.Date(start.date, format="%Y%m%d", tz="GMT"),
        to=as.Date(end.date, format="%Y%m%d", tz="GMT"),
        by="day"),
    function(x) format(x, "%Y%m%d", tz="GMT")
  )

  return(date.range)
}

.tiq.data.defaultTIFields = c("entity", "type", "direction", "source", "notes", "date")


# TI and Population loading data functions --------------------------------

#' Fetches the data contained in the Threat Intelligence dataset contained in
#' 'category' and 'group' on the 'date' specified.
#'
#' @param category currently one of "raw" or "enriched"
#' @param group the intel group you want the path for
#' @param date a date string in the format "YYYYMMDD", or NULL if you want the
#'          date the 'database' has available
#' @return data.table' with at least '.tiq.data.defaultTIFields'
#' @export
loadTI <- function(category, group, date=NULL) {
  return(loadData(category, group, date, valid.fields=.tiq.data.defaultTIFields))
}

#' Fetches the data contained in the Population dataset contained in
#' 'pop.group', with the 'pop.id' as the key on the 'date' specified.
#'
#' @param pop.group the intel group you want the path for
#' @param pop.id the key of the population dataset. If not present in the dataset
#'        the function will return NULL.
#' @param date a date string in the format "YYYYMMDD", or NULL if you want the
#'        date the 'database' has available
#' @return 'data.table' with 'pop.id' and 'totalIPs'
#' @export
loadPopulation <- function(pop.group, pop.id, date = NULL) {
  pop.data = loadData("population", pop.group, date, valid.fields=pop.id)
  if (!is.null(pop.data)) {
    pop.data %>%
      mutate(totalIPs=as.numeric(totalIPs)) %>%
      data.table %>%
      list -> pop.data
    names(pop.data) <- pop.group
  }
  return(pop.data)
}

#' Lower level function helper for loadTI and loadPopulation. Should not be used
#' directly
#'
#' @param category currently one of "raw", "enriched" or "population"
#' @param group the intel group you want the path for
#' @param datea date string in the format "YYYYMMDD", or NULL if you want the
#'        date the 'database' has available
#' @param valid.fields: the fields we should look for in the dataset to validate that
#'        it is of the correct type
#' @return a 'data.table'
loadData <- function(category, group, date=NULL, valid.fields=character(0)) {

  test_that("loadData: unable to open data - 'category' or 'group' are invalid", {
    expect_that(class(category), equals("character"))
    expect_that(class(group), equals("character"))
  })

  # If we do not have a date, we get the latest one we have available
  # The dates are in "%Y%M%d" format, so they can be lexicographically ordered
  if (is.null(date)) {
    date = max(getAvailableDates(category, group))
  }

  # Getting the data path
  ti.path = getDirPath(category, group)
  ti.file = file.path(ti.path, paste0(date, ".csv.gz"))

  if (!file.exists(ti.file)) {
    flog.warn("loadTI: path '%s' is invalid. No data available on date '%s'.",
              ti.file, date)
    return(NULL)
  }

  # Getting and formatting the data
  ti.dt = as.data.table(read.csv(ti.file, header=T, na.strings="",
                                 stringsAsFactors=F, colClasses="character"))

  # Some sanity checking - is this a data.table and does it have all the fields
  # we recognize?
  if (!is.data.table(ti.dt) || !all(valid.fields %chin% names(ti.dt))) {
    flog.warn("loadData: Data loaded from path '%s' is invalid.",
              ti.file)
    return(NULL)
  }

  return(ti.dt)
}
