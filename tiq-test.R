## tiq-test.R
##
## Functions for the Threat Intelligence IQ Tests. These should be used for the different
## evaluations to be run on the TI feeds
## For more information, please refer to the README at
## https://github.com/mlsecproject/tiq-test/

source("utils/log-config.R")
source("utils/tiq-data.R")
source("utils/tiq-helper.R")

library(reshape2)
library(ggplot2)

################################################################################
## NOVELTY Test
##
## The novelty test is about how many indicators are being added and removed
## on each passing day on a specific TI feed. The comparisons are done on each
## 'source' over the multiple days they are available
################################################################################
# tiq.test.noveltyTest - returns a 'list'
# Calculates the novelty test results for a specific 'group' of TI datasets from
# the 'start.date' to the 'end.date'. You need at least 2 days to have a comparison.
# You can alternatively select what sources will be a part of the test by
# providing a character vector to 'select.sources'
# - group: the name of the dataset group. This test works exclusively on "raw"
# - start.date: the beginning date for the test
# - end.date: the end date for the test
# - select.sources: a chararacter vector of the sources on the dataset you want
#                   to be a part of the test, or NULL for all of them
tiq.test.noveltyTest <- function(group, start.date, end.date, select.sources=NULL) {

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

      ## Performing the novelty test only on the sources we select
      if (!is.null(select.sources)) {
        split.names = intersect(select.sources, names(split.ti))
      } else {
        split.names = names(split.ti)
      }

      for (name in split.names) {
        ti.count[[name]][[str.date]] = nrow(split.ti[[name]])
        if (!is.null(prev.split.ti) && !is.null(prev.split.ti[[name]])) {
          ti.added.ratio[[name]][[str.date]] = tiq.helper.differenceCount(split.ti[[name]], prev.split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]
          ti.churn.ratio[[name]][[str.date]] = tiq.helper.differenceCount(prev.split.ti[[name]], split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]
        }
      }
    } else {
      split.ti = NULL
    }
    prev.split.ti = split.ti
  }

  return(list(ti.count=ti.count, ti.added.ratio=ti.added.ratio, ti.churn.ratio=ti.churn.ratio))
}

# tiq.test.plotNoveltyTest - returns nothing (but plots the graph)
# Plots the results of the novelty test in a (mostly) clear graph for comparisons
# - novelty: the output of the 'tiq.test.noveltyTest' function
# - title: a title for your plot
# - plot.sources: a chararacter vector of the sources on the novelty test you want
#                 to be a part of the plot, or NULL for all of them
tiq.test.plotNoveltyTest <- function(novelty, title = "Novelty Test Plot", plot.sources=NULL) {

  test_that("tiq.test.plotNoveltyTest: parameters must have correct types", {
    expect_that(class(novelty), equals("list"))
    expect_that(class(novelty$ti.count), equals("list"))
    expect_that(class(novelty$ti.added.ratio), equals("list"))
    expect_that(class(novelty$ti.churn.ratio), equals("list"))
  })

  ## Plotting the data (to be improved)
  if (is.null(plot.sources)) {
    plot.sources = names(novelty$ti.added.ratio)
  }

  rows = ifelse(length(plot.sources) > 3, 3, length(plot.sources))
  cols = ifelse(length(plot.sources) > 3, 1 + (length(plot.sources) %/% 3), 1)

  par(mfrow=c(rows,cols))

  for (name in plot.sources) {
    plot(novelty$ti.added.ratio[[name]], type="l", col="blue",
         ylim=c(min(-novelty$ti.churn.ratio[[name]]), max(novelty$ti.added.ratio[[name]])),
         xlab="Number of days", ylab="Ratio of Change per Day",
         main=paste0("Source name: ", name, "\nAvg size: ", floor(mean(novelty$ti.count[[name]]))))
    lines(-novelty$ti.churn.ratio[[name]], type="l", col="red")
    abline(h=0)
  }
}

################################################################################
## OVERLAP Test
##
## The overlap test is about how many indicators are present and repeted on
## multiple TI feeds. The comparisons are done on between each "source" selected
## on the same group on a specific day
################################################################################
# tiq.test.overlapTest - returns a numeric matrix with the overlap ratios
# - group: the name of the dataset group. Must exist on the selected 'type'
# - date: the date you want the test to run in
# - type: The overlap test can take into consideration the FQDN sources as
#          the original entities ("raw"), or as the extracted IPv4 fields from
#          the enriched entities ("enriched")
# - select.sources: a chararacter vector of the sources on the dataset you want
#                   to be a part of the test, or NULL for all of them
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
      overlap.count = tiq.helper.overlapCount(split.ti[[select.sources[ti]]],
                                              split.ti[[select.sources[overlap]]])
      overlap.matrix[ti,overlap] = overlap.count /
                                   length(unique(split.ti[[select.sources[ti]]]$entity))
    }
  }

  return(overlap.matrix)
}

# tiq.test.plotOverlapTest - returns a ggplot2 object (plots when printed)
# Plots the results of the overlap test in a (mostly) clear heatmap for comparisons
# - overlap: the output of the 'tiq.test.OverlapTest' function
# - title: a title for your plot
# - plot.sources: a chararacter vector of the sources on the novelty test you want
#                 to be a part of the plot, or NULL for all of them
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

################################################################################
## POPULATION Tests
################################################################################
# tiq.test.extractPopulationFromIT - returns a 'list' of population data.tables
# Returns multiple population data.tables calculated using the sources on the
# "enriched" TI dataset on 'date'. Use 'group' to select the dataset, and
# 'pop.id' for the population key aggregation metric. 'split.ti' and
# 'select.sources' control the output
# - group: the name of the dataset group. Must exist on "enriched" category
# - date: the date you want to use
# - pop.id: the key of the population dataset to generate. Can be multiple keys
# - split.ti: if TRUE, creates a popoulation for each source and returns a list
#             with the sources as IDs. Otherwise, returns a list with a single
#             element with the group as the ID.
# - select.sources: a chararacter vector of the sources on the dataset you want
#                   to be a part of the test, or NULL for all of them. Only
#                   applicable if split.ti = TRUE.
tiq.test.extractPopulationFromTI <- function(group, pop.id, date, split.ti = TRUE,
                                             select.sources=NULL) {
  # Parameter checking
  test_that("tiq.test.extractPopulationFromTI: parameters must have correct types", {
    expect_that(class(category), equals("character"))
    expect_that(class(group), equals("character"))
    expect_that(class(pop.id), equals("character"))
    expect_that(class(date), equals("Date"))
    expect_that(class(split.ti), equals("logical"))
  })

  # Loading the data from the specific date
  str.date = format(date, format="%Y%m%d")
  ti.dt = tiq.data.loadTI("enriched", group, str.date)
  if (is.null(ti.dt)) {
    msg = sprintf("tiq.data.extractPopulationFromTI: unable to locate TI for '%s', '%s', '%s'",
                  "enriched", group, ifelse(is.null(date), "NULL", date))
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

  if (split.ti) {
    ti.data = split(ti.dt, ti.dt$source)
  } else {
    ti.data = list(ti.dt)
    names(ti.data) <- group
  }

  ## Performing the pop generation only on the sources we select
  if (!is.null(select.sources)) {
    select.sources = intersect(select.sources, names(ti.data))
    ti.data = ti.data[select.sources]
  }

  generatePopulation <- function(ti) {
    setkey(ti, entity)
    ti = unique(ti)
    pop.ti = ti[, list(totalIPs=.N), by=pop.id]
    setkeyv(pop.ti, pop.id)
  }

  pop.data = lapply(ti.data, generatePopulation)

  return(pop.data)
}

# tiq.test.plotPopulationBars
# Plots a population bar chart for simple comparisons
# - pop.data: the population data table to be plotted
# - pop.id: the id of the population dataset
# - table.size: the number of bars on the graph. Try not to go too crazy!
# - title: a title for your plot
# - plot.sources: a chararacter vector of the sources on the novelty test you want
#                 to be a part of the plot, or NULL for all of them
tiq.test.plotPopulationBars <- function(pop.data, pop.id, table.size=10,
                                        title="", plot.sources=NULL) {
  # Parameter checking
  test_that("tiq.test.plotPopulationBars: parameters must have correct types", {
    expect_that(class(pop.data), equals("list"))
    expect_that(class(pop.id), equals("character"))
    expect_that(class(table.size), equals("numeric"))
    expect_that(class(title), equals("character"))
  })

  ## Plotting the data (to be improved)
  if (is.null(plot.sources)) {
    plot.sources = names(pop.data)
  }

  ## ggplot2 facets?
  ## Also, issue with asname namesize
  rows = ifelse(length(plot.sources) > 3, 3, length(plot.sources))
  cols = ifelse(length(plot.sources) > 3, 1 + (length(plot.sources) %/% 3), 1)
  par(mfrow=c(rows,cols))

  for (name in plot.sources) {
    pop = pop.data[[name]]
    pop[, totalIPs := totalIPs / sum(pop$totalIPs)]
    pop = pop[order(totalIPs, decreasing=TRUE)]
    if (table.size > 0) pop = pop[1:table.size]
    pop = pop[order(totalIPs, decreasing=FALSE)]

    barplot(height=pop$totalIPs, names.arg=pop[[pop.id]], main=title, col="red",
            xlab=paste0("IP Ratio (",name,")"), horiz=TRUE, las=1, cex.lab=0.75, cex.names=0.75)
    grid()
  }
}

# tiq.test.populationInference - returns a data.table
# Runs an inference-based comparison between the reference population ('ref.pop')
# and the test population ('test.pop'), based on the 'pop.id' key. You should use
# the exact option when the ref.pop was obtained from a "population" dataset, and
# the other one when it is extracted from another TI feed. Also, the "not-exact"
# test loses a lot of precision if the numbers are too low, so stick with the
# Top X members od the test.pop population ideally
#
# - ref.pop - a population dataset (not on a list) from the reference population
# - test.pop - a population dataset (not on a list) to compare
# - pop.id: the id of the population dataset. Does not support multiples ATM
# - exact: When this is TRUE (the default), the function will execute an
#          exact binomial test with the proportion on ref.pop. This should
#          only be the case when ref.pop is the ACTUAL population (e.g. from
#          MaxMind GeoIP database). Otherwise, set this to FALSE for a
#          chi-squared test to compare the different proportions
# - top: the X top members of the test.pop we want to test. Set to -1 for all.
tiq.test.populationInference <- function(ref.pop, test.pop, pop.id,
                                         exact = TRUE, top=25) {
  ## Parameter checking

  # Lets create copies of the data.tables, since we are messing with them
  ref.pop = copy(ref.pop)
  test.pop = copy(test.pop)

  # Reordering the test population so I can get the "top" entries for the test
  # Also, getting rid of any eventual NAs on pop.id (they happen)
  test.pop.total = sum(test.pop$totalIPs, na.rm=T)
  test.pop = test.pop[!is.na(test.pop[[pop.id]])]
  test.pop = test.pop[order(totalIPs, decreasing=T)]
  if (top > 0) {
    test.pop = test.pop[1:top]
  }

  # Calculating the proportion from the reference population so we can use on
  # the exact binomial test and/or the chi-squared test
  ref.pop.total = sum(ref.pop$totalIPs, na.rm=T)
  ref.pop[, pop.ratio := totalIPs / ref.pop.total]
  setnames(ref.pop, "totalIPs", "refIPs")

  # Merging the datasets for the test. If for any of the cases the reference counts
  # are NA (the specific id is not present in the reference population), they are
  # removed from the test
  test.pop = merge(test.pop, ref.pop, by=pop.id, all.x=T, all.y=F)
  test.pop = test.pop[!is.na(pop.ratio)]

  if (exact) { # Exact binomial test
    retval = mapply(binom.test, test.pop$totalIPs, test.pop.total, test.pop$pop.ratio,
                   SIMPLIFY=F, USE.NAMES=F)
    retval = lapply(retval, function(htest) {
      if (!is.null(htest$null.value)) {
        htest$conf.int = htest$conf.int - htest$null.value
      }
      return(htest)
    })
    names(retval) <- test.pop[[pop.id]]
  } else { # Chi-squared proportion test
    sucesses = mapply(c, test.pop$totalIPs, test.pop$refIPs, SIMPLIFY=F,
                      USE.NAMES=F)
    totals = list(c(test.pop.total, ref.pop.total))
    retval = suppressWarnings(mapply(prop.test, sucesses, totals, SIMPLIFY=F, USE.NAMES=F))
    names(retval) <- test.pop[[pop.id]]
  }

  id = names(retval)
  conf.int.start = sapply(retval, function(htest) return(htest$conf.int[1]), USE.NAMES=F)
  conf.int.end = sapply(retval, function(htest) return(htest$conf.int[2]), USE.NAMES=F)
  p.value = sapply(retval, function(htest) return(htest$p.value), USE.NAMES=F)

  dt.retval = data.table(id, conf.int.start, conf.int.end, p.value)
  setnames(dt.retval, "id", pop.id)

  return(dt.retval)
}
