## log-config.R
##
## Handles configuration of the logging output for tiq-test

## Requires: Usage of `futile-logger` package
# library(futile.logger)

# Sets futile.logger formatter that includes timezone and the PID
flog.layout(
  function(level, msg, ...) {
    the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    if (!is.null(substitute(...)))  msg <- sprintf(msg, ...)
    sprintf("%s [%s] pid=%i %s\n", names(level), the.time, Sys.getpid(), msg)
  },
  name='ROOT'
)

# Sets futile.logger output to both the console and an external file
flog.appender(
  function(line) {
    logdirname = file.path(getwd(), "log")
    isdir = file.info(logdirname)$isdir
    if (is.na(isdir)) dir.create(logdirname)
    cat(line, file=file.path(logdirname, format(Sys.Date(), "tiq-test-%Y%m%d.log")), append=TRUE)
    cat(line)
  }
)
