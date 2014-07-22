## tiq-test.R
##
##

source("utils/log.config.R")
source("utils/tiq-data.R")

#####
## CONFIGURATION
##
enrich.group = "public_inbound"
start.date = as.Date("20140615", format="%Y%M%d")
end.date = as.Date("20140715", format="%Y%M%d")

# tiq.test.noveltyTest
tiq.test.noveltyTest <- function(enrich.group, start.date, end.date) {

}
