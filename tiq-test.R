## tiq-test.R
##
##

source("utils/log-config.R")
source("utils/tiq-data.R")

library(reshape2)
library(ggplot2)

# tiq.test.noveltyTest
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

  return(list(ti.count=ti.count, ti.added.ratio=ti.added.ratio, ti.churn.ratio=ti.churn.ratio))
}

# tiq.test.noveltyTest
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

# tiq.test.plotOverlapTest
#
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

tiq.test.extractPopulationFromTI <- function(category, group, pop.id, date,
                                             split.ti = TRUE, select.sources=NULL) {

  test_that("tiq.test.extractPopulationFromTI: parameters must have correct types", {
    expect_that(class(category), equals("character"))
    expect_that(class(group), equals("character"))
    expect_that(class(pop.id), equals("character"))
    expect_that(class(date), equals("Date"))
    expect_that(class(split.ti), equals("logical"))
  })

  # Loading the data from the specific date
  str.date = format(date, format="%Y%m%d")
  ti.dt = tiq.data.loadTI(category, group, str.date)
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
#
tiq.test.plotPopulationBars <- function(pop.data, pop.id, table.size=10,
                                        title="",
                                        plot.sources=NULL) {

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


#####
## Simple validation code
##
if (F) {
  group = "public_inbound"
  start.date = as.Date("20140701", format="%Y%m%d")
  end.date = as.Date("20140715", format="%Y%m%d")


  aa = tiq.test.noveltyTest("public_outbound", start.date, end.date, select.sources=NULL)
  tiq.test.plotNoveltyTest(aa)
  aa2 = tiq.test.noveltyTest("public_outbound", start.date, end.date,
                             select.sources=c("alienvault", "zeus"))
  tiq.test.plotNoveltyTest(aa2)
  bb = tiq.test.noveltyTest("public_inbound", start.date, end.date, select.sources=NULL)
  tiq.test.plotNoveltyTest(bb)
}

if (F) {
  group = "public_outbound"
  type = "enriched"
  end.date = as.Date("20140715", format="%Y%m%d")

  overlap = tiq.test.overlapTest(group, end.date, type, select.sources=NULL)
  print(overlap)
  overlap.plot = tiq.test.plotOverlapTest(overlap, title=paste0("OverlapTest - ", group, " - ", end.date))
  print(overlap.plot)
}

if (F) {
  category = "enriched"
  group = "public_outbound"
  pop.id = "country"
  date=NULL
  pop.group = "mmgeo"
  end.date = as.Date("20140711", format="%Y%m%d")

  pop = tiq.test.extractPopulationFromTI(category, group, "country", end.date,
                                         select.sources=NULL)
  tiq.test.plotPopulationBars(pop, "country")

  pop = tiq.test.extractPopulationFromTI(category, group, c("asnumber", "asname"), end.date,
                                         select.sources=NULL)
  tiq.test.plotPopulationBars(pop, "asname")

  pop = tiq.test.extractPopulationFromTI(category, group, c("asnumber", "asname"), end.date,
                                         select.sources=NULL,
                                         split.ti=FALSE)
  pop.mm = tiq.data.loadPopulation("mmasn", c("asnumber", "asname"))
  tiq.test.plotPopulationBars(c(pop, pop.mm), "asname")

  pop = tiq.test.extractPopulationFromTI(category, group, "country", end.date,
                                         select.sources=NULL,
                                         split.ti=FALSE)
  pop.mm = tiq.data.loadPopulation("mmgeo", "country")
  tiq.test.plotPopulationBars(c(pop, pop.mm), "country")

  pop = tiq.test.extractPopulationFromTI(category, "public_inbound", "country", end.date,
                                         select.sources=NULL,
                                         split.ti=FALSE)
  pop.mm = tiq.data.loadPopulation("mmgeo", "country")
  tiq.test.plotPopulationBars(c(pop, pop.mm), "country")


  print(pop)
}

