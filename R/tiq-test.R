# NOVELTY Test ------------------------------------------------------------

#' The novelty test is about how many indicators are being added and removed
#' on each passing day on a specific TI feed.
#'
#' The comparisons are done on each 'source' over the multiple days they are
#' available. \cr
#' \cr
#' Calculates the novelty test results for a specific 'group' of TI datasets from
#' the 'start.date' to the 'end.date'. You need at least 2 days to have a comparison.
#' You can alternatively select what sources will be a part of the test by
#' providing a character vector to 'select.sources'\cr
#' \cr
#' Defaults to showing a progress bar. Set \code{.progress} to \code{FALSE} when knitting
#'
#' @param group the name of the dataset group. This test works exclusively on "raw"
#' @param start.date the beginning date for the test
#' @param end.date the end date for the test
#' @param select.sources a chararacter vector of the sources on the dataset you want
#'        to be a part of the test, or NULL for all of them
#' @param .progress show a progress bar? (default: \code{TRUE} - show a progress bar)
#' @return a 'list'
#' @export
noveltyTest <- function(group, start.date, end.date, select.sources=NULL, .progress=TRUE) {
  # Parameter checking
  test_that("tiq.test.noveltyTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_match(start.date, "^[[:digit:]]{8}$", info="must be a date (YYYYMMDD)")
    expect_match(end.date, "^[[:digit:]]{8}$", info="must be a date (YYYYMMDD)")
  })

  # Calculating the date range for the test form start and end dates
  date.range = getDateSequence(start.date, end.date)

  prev.split.ti = NULL
  ti.count = list()
  ti.added.ratio = list()
  ti.churn.ratio = list()

  if (.progress) pb = txtProgressBar(min = 1, max = length(date.range), style = 3)
  if (.progress) pb_inc = 0
  ## For each date, get the respective RAW TI feed and calculate the counts
  ## per source that is a part of the group
  for (str.date in date.range) {
    if (.progress) pb_inc = pb_inc + 1
    if (.progress) setTxtProgressBar(pb = pb, value = pb_inc)
    ## Loading the RAW TI from the respective date and separating by source
    ti.dt = loadTI("raw", group, date=str.date)
    if (!is.null(ti.dt)) {
      split.ti = split(ti.dt, ti.dt$source)

      ## Performing the novelty test only on the sources we select
      if (!is.null(select.sources)) {
        split.names = intersect(select.sources, names(split.ti))
      } else {
        split.names = names(split.ti)
      }

      for (name in split.names) {
        setkey(split.ti[[name]], entity)
        split.ti[[name]] = unique(split.ti[[name]])

        ti.count[[name]][[str.date]] = nrow(split.ti[[name]])
        if (!is.null(prev.split.ti) && !is.null(prev.split.ti[[name]])) {
          ti.added.ratio[[name]][[str.date]] = tiq.helper.differenceCount(split.ti[[name]], prev.split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]
          ti.churn.ratio[[name]][[str.date]] = tiq.helper.differenceCount(prev.split.ti[[name]], split.ti[[name]]) /
                                               ti.count[[name]][[str.date]]

          ## This adjustment is necessary to compensate for data collection issues
          ## we had on outbound feeds. You can't really change more then everything you got
          if (ti.added.ratio[[name]][[str.date]] > 0.6) ti.added.ratio[[name]][[str.date]] = 0
          if (ti.churn.ratio[[name]][[str.date]] > 1) ti.churn.ratio[[name]][[str.date]] = 0
        }
      }
    } else {
      split.ti = NULL
    }
    prev.split.ti = split.ti
  }
  if (.progress) close(pb)

  return(list(ti.count=ti.count, ti.added.ratio=ti.added.ratio, ti.churn.ratio=ti.churn.ratio))
}

#' Plots the results of the novelty test in a (mostly) clear graph for comparisons
#'
#' @param novelty the output of the 'tiq.test.noveltyTest' function
#' @param plot.sources a chararacter vector of the sources on the novelty test you want
#'        to be a part of the plot, or NULL for all of them
#' @export

plotNoveltyTest <- function(novelty, sources=NULL) {

  test_that("tiq.test.plotNoveltyTest: parameters must have correct types", {
    expect_that(class(novelty), equals("list"))
    expect_that(class(novelty$ti.count), equals("list"))
    expect_that(class(novelty$ti.added.ratio), equals("list"))
    expect_that(class(novelty$ti.churn.ratio), equals("list"))
  })

  if (is.null(sources)) {
    plot.sources = names(novelty$ti.added.ratio)
  }

  tmp <- as.data.frame(novelty$ti.added.ratio)
  tmp$date <- rownames(tmp)
  rownames(tmp) <- NULL
  added_ratio <- gather(tmp, source, added_ratio, -date)

  tmp <- as.data.frame(novelty$ti.churn.ratio)
  tmp$date <- rownames(tmp)
  rownames(tmp) <- NULL
  churn_ratio <- gather(tmp, source, churn_ratio, -date)

  tmp <- as.data.frame(novelty$ti.count)
  tmp$date <- rownames(tmp)
  rownames(tmp) <- NULL
  ti_count <- gather(tmp, source, ti_count, -date)

  tmp <- merge(merge(added_ratio, churn_ratio), ti_count)

  tmp$churn_ratio <- -tmp$churn_ratio
  tmp$churn_color <- "#35978f"
  tmp$added_color <- "#bf812d"

  tmp %>%
    filter(source %in% plot.sources) %>%
    group_by(source) %>%
    mutate(avg_size=floor(mean(ti_count)),
           label=sprintf("Source Name: %s\nAvg. Size: %s",
                         source, comma(avg_size))) -> tmp

  gg <- ggplot(tmp, aes(x=date))
  gg <- gg + geom_bar(stat="identity", aes(y=added_ratio, fill=added_color))#, fill="#bf812d")
  gg <- gg + geom_bar(stat="identity", aes(y=churn_ratio, fill=churn_color))#, fill="#35978f")
  gg <- gg + geom_hline(yintercept=0, color="black", size=0.5)
  gg <- gg + scale_y_continuous(labels=comma)
  gg <- gg + scale_fill_identity(name="Variation", labels=c("Added", "Churn"), guide="legend")
  gg <- gg + facet_wrap(~label, ncol=2, scales="free_y")
  gg <- gg + labs(y="Change Ratio per Day", x=NULL, title="TIQ Novelty Test")
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x=element_text(angle=90, hjust=1, size=7))
  gg <- gg + theme(strip.background=element_blank())

  gg

}

# OVERLAP Test ------------------------------------------------------------
#
#' The overlap test is about how many indicators are present and repeted on
#' multiple TI feeds.
#'
#' The comparisons are done on between each "source" selected
#' on the same group on a specific day
#'
#' @param group the name of the dataset group. Must exist on the selected 'type'
#' @param date the date you want the test to run in
#' @param type The overlap test can take into consideration the FQDN sources as
#'        the original entities ("raw"), or as the extracted IPv4 fields from
#'        the enriched entities ("enriched")
#' @param select.sources a chararacter vector of the sources on the dataset you want
#'        to be a part of the test, or NULL for all of them
#' @return a numeric matrix with the overlap ratios
#' @export
overlapTest <- function(group, date, type="raw", select.sources=NULL) {

  test_that("tiq.test.overlapTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_match(date, "^[0123456789]{8}$", info="must be a date (YYYYMMDD)")
    expect_that(class(type), equals("character"))
  })

  # Loading the data from the specific date
  ti.dt = loadTI(type, group, date=date)
  split.ti = split(ti.dt, ti.dt$source)

  ## Performing the overlap test only on the sources we select
  if (is.null(select.sources)) {
    select.sources = unique(ti.dt$source)
  }

  overlap.matrix = matrix(nrow=length(select.sources), ncol=length(select.sources),
                          dimnames=list(select.sources, select.sources))

  # Removing duplicated entries from each source
  for (ti in 1:length(select.sources)) {
    setkey(split.ti[[select.sources[ti]]], entity)
    split.ti[[select.sources[ti]]] = unique(split.ti[[select.sources[ti]]])
  }

  # Calculating the overlap for each pairing
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

#' Plots the results of the overlap test in a (mostly) clear heatmap for comparisons
#'
#' @param overlap the output of the 'tiq.test.OverlapTest' function
#' @param title a title for your plot
#' @param plot.sources a chararacter vector of the sources on the novelty test you want
#'        to be a part of the plot, or NULL for all of them
#' @return ggplot2 object (plots when printed)
#' @export
plotOverlapTest <- function(overlap, title="Overlap Test Plot", plot.sources=NULL) {

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

  gg <- ggplot(plot.data, aes(x=Var1, y=Var2))
  gg <- gg + geom_tile(aes(fill=value), color="#e3e3e3", size=0.5)
  gg <- gg + coord_equal()
  gg <- gg + scale_fill_distiller(palette="YlOrBr", name="%\nOverlap", labels=percent)
  gg <- gg + labs(x="Source (is contained)", y="Source (contains)", title=title)
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
  gg <- gg + theme(axis.text.y = element_text(hjust = 1, size=12))
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.border=element_blank())

  return(gg)

}


# POPULATION Tests --------------------------------------------------------

#' Returns multiple population data.tables calculated using the sources on the
#' "enriched" TI dataset on 'date'.
#'
#' Use 'group' to select the dataset, and 'pop.id' for the population key
#' aggregation metric. 'split.ti' and 'select.sources' control the output
#'
#' @param group the name of the dataset group. Must exist on "enriched" category
#' @param date the date you want to use
#' @param pop.id the key of the population dataset to generate. Can be multiple keys
#' @param split.ti if TRUE, creates a popoulation for each source and returns a list
#'        with the sources as IDs. Otherwise, returns a list with a single
#'        element with the group as the ID.
#' @param select.sources a chararacter vector of the sources on the dataset you want
#         to be a part of the test, or NULL for all of them. Only
#         applicable if split.ti = TRUE.
#' @return 'list' of population data.tables
#' @export
extractPopulationFromTI <- function(group, pop.id, date, split.ti = TRUE,
                                             select.sources=NULL) {
  # Parameter checking
  test_that("tiq.test.extractPopulationFromTI: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_that(class(pop.id), equals("character"))
    expect_match(date, "^[0123456789]{8}$", info="must be a date (YYYYMMDD)")
    expect_that(class(split.ti), equals("logical"))
  })

  # Loading the data from the specific date
  ti.dt = loadTI("enriched", group, date)
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

#' Plots a population bar chart for simple comparisons
#'
#' @param pop.data the population data table to be plotted
#' @param pop.id the id of the population dataset
#' @param table.size the number of bars on the graph. Try not to go too crazy!
#' @param title a title for your plot
#' @param plot.sources a chararacter vector of the sources on the novelty test you want
#'        to be a part of the plot, or NULL for all of them
#' @export
plotPopulationBars <- function(pop.data, pop.id, table.size=10,
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

  # Preparing the data and calculating the max proportion we have
  max.pop = 0.0
  for (name in plot.sources) {
    pop = pop.data[[name]]
    pop[, totalIPs := totalIPs / sum(pop$totalIPs)]
    if (max(pop$totalIPs) > max.pop) max.pop = max(pop$totalIPs)
  }

  # Creating the Plots
  plots = list()
  for (name in plot.sources) {

    pop = copy(pop.data[[name]])
    pop = pop[order(totalIPs, decreasing=TRUE)]

    if (table.size > 0) pop = pop[1:table.size]

    pop = pop[order(totalIPs, decreasing=FALSE)]

    setnames(pop, pop.id, "pop.id")

    gg <- ggplot(pop, aes(x=reorder(pop.id,totalIPs), y=totalIPs))
    gg <- gg + geom_bar(stat="identity", fill="#543005", width=0.65)
    gg <- gg + scale_x_discrete(expand=c(0,0), name="", labels=sprintf("%s (%.2f)", pop$pop.id, pop$totalIPs))
    gg <- gg + scale_y_continuous(expand=c(0,0), limits=c(0.0, max.pop))
    gg <- gg + labs(y="IP Ratio", title=paste0("Population Summary by ", pop.id, " (",name,")"))
    gg <- gg + coord_flip()
    gg <- gg + theme_bw()
    gg <- gg + theme(axis.text = element_text(size=12, colour="black"))
    gg <- gg + theme(axis.ticks.y = element_blank())
    gg <- gg + theme(panel.grid=element_blank())
    gg <- gg + theme(panel.border=element_blank())

    plots[[name]] = gg

  }

  ## Let's try to organize them in on top of each other
  plots = c(plots, list(ncol=1))
  do.call(grid.arrange, plots)
}

#' Runs an inference-based comparison between the reference population ('ref.pop')
#' and the test population ('test.pop'), based on the 'pop.id' key.
#'
#' You should use the exact option when the ref.pop was obtained from a
#' "population" dataset, and the other one when it is extracted from another
#' TI feed. Also, the "not-exact" test loses a lot of precision if the numbers
#' are too low, so stick with the Top X members od the test.pop population ideally
#'
#' @param ref.pop a population dataset (not on a list) from the reference population
#' @param test.pop a population dataset (not on a list) to compare
#' @param pop.id the id of the population dataset. Supports multiple ids, indexes
#'        the output by the first one
#' @param exact When this is TRUE (the default), the function will execute an
#'          exact binomial test with the proportion on ref.pop. This should
#'          only be the case when ref.pop is the ACTUAL population (e.g. from
#'          MaxMind GeoIP database). Otherwise, set this to FALSE for a
#'          chi-squared test to compare the different proportions
#' @param top the X top members of the test.pop we want to test. Set to -1 for all.
#' @return a data.table
#' @export
populationInference <- function(ref.pop, test.pop, pop.id,
                                         exact = TRUE, top=25) {

  # Lets create copies of the data.tables, since we are messing with them
  ref.pop = copy(ref.pop)
  test.pop = copy(test.pop)

  # Reordering the test population so I can get the "top" entries for the test
  # Also, getting rid of any eventual NAs on pop.id (they happen)
  test.pop.total = sum(test.pop$totalIPs, na.rm=T)
  test.pop = test.pop[!is.na(test.pop[[pop.id[1]]])]
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
    names(retval) <- test.pop[[pop.id[1]]]
  } else { # Chi-squared proportion test
    sucesses = mapply(c, test.pop$totalIPs, test.pop$refIPs, SIMPLIFY=F,
                      USE.NAMES=F)
    totals = list(c(test.pop.total, ref.pop.total))
    retval = suppressWarnings(mapply(prop.test, sucesses, totals, SIMPLIFY=F, USE.NAMES=F))
    names(retval) <- test.pop[[pop.id[1]]]
  }

  id = names(retval)
  conf.int.start = sapply(retval, function(htest) return(htest$conf.int[1]), USE.NAMES=F)
  conf.int.end = sapply(retval, function(htest) return(htest$conf.int[2]), USE.NAMES=F)
  p.value = sapply(retval, function(htest) return(htest$p.value), USE.NAMES=F)

  dt.retval = data.table(id, conf.int.start, conf.int.end, p.value)
  setnames(dt.retval, "id", pop.id[1])

  return(dt.retval)
}


# AGING Tests -------------------------------------------------------------

#' Calculates the number of times an indicator is repeated on a feed throughout
#'
#' Calculates the number of times an indicator is repeated on a feed throughout
#' the days from 'start.date' to 'end.date' Use 'group' and 'type' to select the
#' dataset. 'split.ti' and 'select.sources' control the output.\cr
#' \cr
#' Defaults to showing a progress bar. Set \code{.progress} to \code{FALSE} when knitting
#'
#' @param group the name of the dataset group. Must exist on the 'type' category
#' @param start.date the beginning date for the test
#' @param end.date the end date for the test
#' @param type The test can take into consideration the FQDN sources as
#'        the original entities ("raw"), or as the extracted IPv4 fields from
#'        the enriched entities ("enriched")
#' @param split.ti if TRUE, creates a popoulation for each source and returns a list
#'        with the sources as IDs. Otherwise, returns a list with a single
#'        element with the group as the ID.
#' @param select.sources a chararacter vector of the sources on the dataset you want
#'        to be a part of the test, or NULL for all of them. Only
#'        applicable if split.ti = TRUE.
#' @param .progress show a progress bar? (default: \code{TRUE} - show a progress bar)
#' @return "agingtest" object
#' @export
agingTest <- function(group, start.date, end.date, type = "raw",
                      split.ti = TRUE, select.sources=NULL,
                      .progress=TRUE) {
  # Parameter checking
  test_that("agingTest: parameters must have correct types", {
    expect_that(class(group), equals("character"))
    expect_match(start.date, "^[[:digit:]]{8}$", info="must be a date (YYYYMMDD)")
    expect_match(end.date, "^[[:digit:]]{8}$", info="must be a date (YYYYMMDD)")
    expect_is(type, "character")
    expect_is(split.ti, "logical")
  })

  # Calculating the date range for the test form start and end dates
  date.range = getDateSequence(start.date, end.date)

  # Initialize the reply we are going to provide
  list.dt.counts = list()

  if (.progress) pb = txtProgressBar(min = 1, max = length(date.range), style = 3)
  if (.progress) pb_inc = 0

  for (date in date.range) {

    if (.progress) pb_inc = pb_inc + 1
    if (.progress) setTxtProgressBar(pb = pb, value = pb_inc)

    flog.debug("agingTest: processing '%s'/'%s' info from '%s'",
                  type, group, ifelse(is.null(date), "NULL", date))
    ti.dt = loadTI("enriched", group, date)
    if (is.null(ti.dt)) next

    ## Splitting and selecting (This needs to be a refactor into ti.data)
    ## Makes no sense to be repeating this code
    if (split.ti) {
      ti.data = split(ti.dt, ti.dt$source)
    } else {
      ti.data = list(ti.dt)
      names(ti.data) <- group
    }
    if (!is.null(select.sources)) {
      select.sources = intersect(select.sources, names(ti.data))
      ti.data = ti.data[select.sources]
    }

    ## Summarizing the data from each subgroup, and creating a data.table that
    ## has a 1 marked on the date I found them
    generateDateInfo <- function(dt_it, date) {
      dt_ret = data.table(entity = unique(dt_it$entity), date = 1)
      setnames(dt_ret, "date", date)
    }
    dateinfo.list = lapply(ti.data, generateDateInfo, date=date)
    dateinfo.names = names(dateinfo.list)

    ## Now, I need to merge this data with the list I already have been creating
    for(name in dateinfo.names) {
      dt_subgroup = list.dt.counts[[name]]
      if (is.null(dt_subgroup)) {
        list.dt.counts[[name]] = dateinfo.list[[name]]
      } else {
        list.dt.counts[[name]] = merge(dt_subgroup, dateinfo.list[[name]], by="entity", all=T)
      }
    }
  }

  if (.progress) close(pb)

  ## Now, we summarize all the data and return the counts
  summarizeAgingInfo <- function(dt_aging) {
    date.names = intersect(date.range, names(dt_aging))
    return(rowSums(dt_aging[, date.names, with=F], na.rm=T))
  }
  retval = lapply(list.dt.counts, summarizeAgingInfo)
  retval[["_agingtest.days"]] = length(date.range)
  class(retval) <- "agingtest"

  return(retval)
}


#' Plots an Aging Test histogram and density plot
#'
#' @param aging.data the aging data object to be plotted. More specifically the output of
#'        tiq.test.agingTest()
#' @param title: a title for your plot. NULL leaves it blank
#' @param plot.sources: a chararacter vector of the sources on the novelty test you want
#'        to be a part of the plot, or NULL for all of them
#' @export
plotAgingTest <- function(aging.data, title=NULL, plot.sources=NULL) {
  # Parameter checking
  test_that("plotAgingTest: parameters must have correct types", {
    expect_is(aging.data, "agingtest")
  })

  # Important for graph alignment
  total.days = aging.data[["_agingtest.days"]]
  aging.data[["_agingtest.days"]] = NULL

  # Selecting the sources to plot
  if (is.null(plot.sources)) {
    plot.sources = names(aging.data)
  }

  tmp <- names(aging.data)
  source_dat <- rbindlist(lapply(tmp, function(x) {
    data.frame(source=sprintf("Source: '%s'", x), value=aging.data[[x]])
  }))

  gg <- ggplot(source_dat, aes(x=value))
  gg <- gg + geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                            binwidth=1, colour="black", fill="#f5f5f5")
  gg <- gg + geom_density(alpha=.2, fill="#01665e")
  gg <- gg + xlim(0, total.days + 1)
  gg <- gg + facet_wrap(~source, scales="free_y")
  gg <- gg + labs(x=NULL, y="Density",
                  title=sprintf("Indicator Age - Sampled Time: %s days", total.days))
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text=element_text(size=12, colour="black"))
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(strip.background=element_blank())
  gg
}
