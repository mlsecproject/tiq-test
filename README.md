tiq-test
========
Threat Intelligence Quotient Test

Code and data repository for the statistical analysis of TI indicator feeds

As seen in:
* BSides LV 2014 - "Measuring the IQ of your threat intelligence feeds"
* DEF CON 22 - "Measuring the IQ of your threat intelligence feeds"

Check out the usage examples of the talks in http://rpubs.com/alexcpsec/tiq-test-Summer2014

Copyright Info
--------------
Copyright 2014 MLSec Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

Licensed under GPLv3 - https://github.com/mlsecproject/tiq-test/blob/master/LICENSE

Requirements
------------
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

Running this on your R console should take care of that:
```
install.packages(c("futile.logger", "data.table", "testthat", "reshape2", "ggplot2"), 
                 repos="http://cran.us.r-project.org")
```
