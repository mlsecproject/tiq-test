tiq is Functions for the Threat Intelligence IQ Tests. These should be used for the different evaluations to be run on the TI feeds.

The following functions are implemented:

-   `agingTest` - Calculates the number of times an indicator is repeated on a feed throughout
-   `extractPopulationFromTI` - Returns multiple population data.tables calculated using the sources on the "enriched" TI dataset on 'date'.
-   `getAvailableDates` - Queries the data repository for the specific 'category' and 'group' of TI or population information and return the dates we have available for that group.
-   `getDirPath` - Builds the path for the intel analysts sources and returns it.
-   `loadData` - Lower level function helper for loadTI and loadPopulation. Should not be used directly
-   `loadPopulation` - Fetches the data contained in the Population dataset contained in 'pop.group', with the 'pop.id' as the key on the 'date' specified.
-   `loadTI` - Fetches the data contained in the Threat Intelligence dataset contained in 'category' and 'group' on the 'date' specified.
-   `noveltyTest` - The novelty test is about how many indicators are being added and removed on each passing day on a specific TI feed.
-   `overlapTest` - The overlap test is about how many indicators are present and repeted on multiple TI feeds.
-   `plotAgingTest` - Plots an Aging Test histogram and density plot
-   `plotNoveltyTest` - Plots the results of the novelty test in a (mostly) clear graph for comparisons
-   `plotOverlapTest` - Plots the results of the overlap test in a (mostly) clear heatmap for comparisons
-   `plotPopulationBars` - Plots a population bar chart for simple comparisons
-   `populationInference` - Runs an inference-based comparison between the reference population ('ref.pop') and the test population ('test.pop'), based on the 'pop.id' key.
-   `setRootPath` - Set data path

### News

-   Version 1.0 released

### Installation

``` r
devtools::install_github("mlsecproject/tiq")
```

### Usage

``` r
library(tiq)

# current verison
packageVersion("tiq")

tiqtest.dir = "/SOME/DATA/DIR/tiq-test-Summer2014"
current.dir = setwd(tiqtest.dir)

setRootPath(file.path(current.dir, "data"))

getAvailableDates("raw", "public_outbound")

outbound.ti = tiq.data.loadTI("raw", "public_outbound", "20140701")

outbound.ti[, list(entity, type, direction, source, date)]
inbound.novelty = noveltyTest("public_inbound", "20140615", "20140630",
                                       select.sources=c("alienvault", "blocklistde",
                                                        "dshield", "charleshaley"))

plotNoveltyTest(inbound.novelty)

overlap = overlapTest("public_inbound", "20140715", "enriched",
                               select.sources=NULL)

overlap.plot = plotOverlapTest(overlap, title="Overlap Test - Inbound Data - 20140715")
print(overlap.plot)


outbound.pop = extractPopulationFromTI("public_outbound", "country",
                                                date = "20140711",
                                                select.sources=NULL, split.ti=F)
inbound.pop = extractPopulationFromTI("public_inbound", "country",
                                               date = "20140711",
                                               select.sources=NULL, split.ti=F)

complete.pop = loadPopulation("mmgeo", "country")
plotPopulationBars(c(inbound.pop, outbound.pop, complete.pop), "country")
```

### Test Results

``` r
library(tiq)
```

    ## Loading required package: futile.logger
    ## Loading required package: testthat
    ## Loading required package: gridExtra
    ## Loading required package: grid
    ## Loading required package: ggplot2
    ## Loading required package: data.table

``` r
library(testthat)

date()
```

    ## [1] "Thu Jan 29 05:18:00 2015"

``` r
test_dir("tests/")
```

    ## basic functionality :
