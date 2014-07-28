## tiq-test.R
##
##

source("utils/log-config.R")
source("utils/tiq-data.R")

library(testthat)
library(reshape2)
library(ggplot2)

# tiq.test.noveltyTest
tiq.test.noveltyTest <- function(group, start.date, end.date, plot.sources=NULL) {

  test_that("tiq.test.noveltyTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_that(class(start.date), equals("Date"))
    expect_that(class(end.date), equals("Date"))
  })

  if (end.date <= start.date) {
    msg = sprintf("tiq.test.noveltyTest: The end.date %s must be later than the start.date %s",
                  as.character(end.date), as.character(start.date))
    flog.error(msg)
    stop(msg)
  }

  prev.split.ti = NULL
  ti.count = list()
  ti.added.ratio = list()
  ti.churn.ratio = list()

  ## For each date, get the respective RAW TI feed and calculate the counts
  ## per source that is a part of the group
  for (date in start.date:end.date) {
    date = as.Date(date, origin="1970-01-01")
    str.date = format(date, format="%Y%m%d")

    ## Loading the RAW TI from the respective date and separating by source
    ti.dt = tiq.data.loadTI("raw", group, date=str.date)
    if (!is.null(ti.dt)) {
      split.ti = split(ti.dt, ti.dt$source)
      for (name in names(split.ti)) {
        ti.count[[name]][[str.date]] = nrow(split.ti[[name]])
        if (!is.null(prev.split.ti) && !is.null(prev.split.ti[[name]])) {
          ti.added.ratio[[name]][[str.date]] = differenceCount(split.ti[[name]], prev.split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]
          ti.churn.ratio[[name]][[str.date]] = differenceCount(prev.split.ti[[name]], split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]
        }
      }
    } else {
      split.ti = NULL
    }
    prev.split.ti = split.ti
  }

  ## Plotting the data (to be improved)
  if (is.null(plot.sources)) {
    plot.sources = names(ti.added.ratio)
  }
  if (length(plot.sources) == 1) {
    par(mfrow=c(1,1))
  } else if (length(plot.sources) == 2) {
    par(mfrow=c(2,1))
  } else {
    par(mfrow=c(3,ceiling(length(plot.sources)/3)))
  }

  for (name in plot.sources) {
    plot(ti.added.ratio[[name]], type="l", col="blue",
         ylim=c(min(-ti.churn.ratio[[name]]), max(ti.added.ratio[[name]])),
         xlab="Number of days since start", ylab="Ratio of Indicator Change",
         main =paste0("Source: ", name, "\navg size - ", floor(mean(ti.count[[name]]))))
    lines(-ti.churn.ratio[[name]], type="l", col="red")
    abline(h=0)
  }

  return(list(ti.count=ti.count, ti.added.ratio=ti.added.ratio, ti.churn.ratio=ti.churn.ratio))

}

# tiq.test.overlapTest
# - type - The overlap test can take into consideration the FQDN sources as
#          the original entities ("raw"), or as the extracted IPv4 fields from
#          the enriched entities ("enriched")
tiq.test.overlapTest <- function(group, date, type="raw", select.sources=NULL) {

  test_that("tiq.test.overlapTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_that(class(date), equals("Date"))
    expect_that(class(type), equals("character"))
  })

  # Loading the data from the specific date
  str.date = format(date, format="%Y%m%d")
  ti.dt = tiq.data.loadTI(type, group, date=str.date)
  split.ti = split(ti.dt, ti.dt$source)

  ## Performing the overlap test only on the sources we select
  if (is.null(select.sources)) {
    select.sources = unique(ti.dt$source)
  }

  overlap.matrix = matrix(nrow=length(select.sources), ncol=length(select.sources),
                          dimnames=list(select.sources, select.sources))

  for (ti in 1:length(select.sources)) {
    for (overlap in 1:length(select.sources)) {
      # For each pairing
      overlap.count = overlapCount(split.ti[[select.sources[ti]]],
                                   split.ti[[select.sources[overlap]]])
      overlap.matrix[ti,overlap] = overlap.count /
                                   length(unique(split.ti[[select.sources[ti]]]$entity))
    }
  }

  return(overlap.matrix)
}



differenceCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("differenceCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(setdiff(test$entity, reference$entity)))
}

overlapCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("overlapCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(intersect(test$entity, reference$entity)))
}

tiq.test.plotOverlapTest <- function(overlap, title="Overlap Test Plot", plot.sources=NULL) {
  if (!is.matrix(overlap) || (dim(overlap)[1] != dim(overlap)[2])) {
    msg = sprintf("tiq.test.plotOverlapTest: 'overlap' parameter mush be a square matrix")
    flog.error(msg)
    stop(msg)
  }

  plot.data = as.data.table(melt(overlap))
  if (!is.null(plot.sources)) {
    plot.data = plot.data[as.character(Var1) %chin% plot.sources]
    plot.data = plot.data[as.character(Var2) %chin% plot.sources]
  }

  qplot(x=Var1, y=Var2, data=plot.data, fill=value, geom="tile",
        xlab="Source", ylab="Source", main=title)

}

#####
## Simple validation code
##
group = "public_inbound"
start.date = as.Date("20140701", format="%Y%m%d")
end.date = as.Date("20140715", format="%Y%m%d")

if (F) {
  aa = tiq.test.noveltyTest("public_outbound", start.date, end.date, plot.sources=NULL)
  bb = tiq.test.noveltyTest("public_inbound", start.date, end.date, plot.sources=NULL)
}

if (F) {
  group = "public_outbound"
  type = "enriched"
  overlap = tiq.test.overlapTest(group, end.date, type, select.sources=NULL)
  overlap.plot = tiq.test.plotOverlapTest(overlap, title=paste0("OverlapTest - ", group, " - ", end.date))

  print(overlap.plot)
}
