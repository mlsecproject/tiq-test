tiq-test - Threat Intelligence Quotient Test
============================================
Dataviz and Statistical Analysis of Threat Intelligence Indicator feeds

### First batch of presentations

These presentations introduced `combine` and `tiq-test` with the 3 initial tests:
* BSides LV 2014 - "Measuring the IQ of your threat intelligence feeds"
* DEF CON 22 - "Measuring the IQ of your threat intelligence feeds"
* OpenDNS S4 IRespond - "Measuring the IQ of your threat intelligence feeds"
* HushCon 2014 - "Measuring the IQ of your threat intelligence feeds"

The data repository and R Markdown source for these talks can be found at https://github.com/mlsecproject/tiq-test-Summer2014

Follow the RPubs file at http://bit.ly/tiqtest-rpubs

### Second batch of presentations

These presentations introduced the Aging and Uniqueness tests:
* nbtcon 2014: "From Threat Intelligence to Defense Cleverness: A Data Science Approach"
* SANS CTI Summit 2015: "From Threat Intelligence to Defense Cleverness: A Data Science Approach"

The data repository and R Markdown source for these talks can be found at https://github.com/mlsecproject/tiq-test-Winter2015

Follow the RPubs file at http://rpubs.com/alexcpsec/tiq-test-Winter2015

## Copyright Info
Copyright 2014-2015 (c) MLSec Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

Licensed under GPLv3 - https://github.com/mlsecproject/tiq-test/blob/master/LICENSE

## Requirements

In order to run these scripts, you will need to install R on your computer. We also suggest using RStudio as an IDE.

* Downloading and Installing R - http://cran.us.r-project.org/
* Downloading and Installing RStudio - http://www.rstudio.com/products/rstudio/download/

Once you have your installation up and running, you will need to make sure you
have some packages installed. The required packages are:

* `futile.logger`
* `data.table`
* `testthat`
* `reshape2`
* `ggplot2`
* `gridExtra`
* `scales`
* `dplyr`
* `tidyr`

Running this on your R console should take care of that of you:
```
install.packages(c("futile.logger", "data.table", "testthat", 
                   "reshape2", "ggplot2", "gridExtra", "scales",
                   "dplyr", "tidyr"),
                 repos="http://cran.us.r-project.org")
```

## How to use these tools

This is not an R package (yet) so it is necessary for you to set the working directory
to the directory where you clone this repository in order for the functions to work.

```
### Some limitations from not being an R package: Setting the Working directory
tiqtest.dir = "../tiq-test"
current.dir = setwd(tiqtest.dir)
source("tiq-test.R")
```

There is also the requirement to set the data directory so the data functions
know where to look.

```
### Setting the root data path to where it should be in this repo
.tiq.data.setRootPath(file.path(current.dir, "data"))
```

Check out usage examples for the tests on the presentation RPubs files:
* http://rpubs.com/alexcpsec/tiq-test-Summer2014-2
* http://rpubs.com/alexcpsec/tiq-test-Winter2015
