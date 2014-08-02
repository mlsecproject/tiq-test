## tiq-helper.R
##
## Helper functions for operations on TI data.tables
##

tiq.helper.differenceCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.differenceCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(setdiff(test$entity, reference$entity)))
}

tiq.helper.overlapCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.overlapCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(intersect(test$entity, reference$entity)))
}
