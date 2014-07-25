## tiq-test.R
##
##

source("utils/log-config.R")
source("utils/tiq-data.R")

library(testthat)

#####
## CONFIGURATION
##
group = "public_inbound"
start.date = as.Date("20140615", format="%Y%M%d")
end.date = as.Date("20140715", format="%Y%M%d")

category = "raw"

# tiq.test.noveltyTest
tiq.test.noveltyTest <- function(group, start.date, end.date) {

  test_that("tiq.test.noveltyTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_that(class(start.date), equals("Date"))
    expect_that(class(end.date), equals("Date"))
  })

  if (end.date <= start.date) {
    msg = "Not Implemented"
    flog.error(msg)
    stop(msg)
  }
  ## For each date, we need to get the data we have


}
