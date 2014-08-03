## tiq-helper.R
##
## Helper functions for operations on TI data.tables
##

library(data.table)

################################################################################
## Operations on TI data.table objects
################################################################################
# tiq.helper.differenceCount - returns 'numeric'
# Returns the number of entities in the 'test' TI data that are not present
# in the 'reference' TI data. This is not commutative, as the differences are
# potentially asymmetric
# - test: The TI data object we are interested on comparing
# - reference: The TI data object we are using as reference for the comparison
tiq.helper.differenceCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.differenceCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(setdiff(test$entity, reference$entity)))
}

# tiq.helper.overlapCount - returns 'numeric'
# Returns the number of entities in the 'test' TI data that are ALSO present
# in the 'reference' TI data. This is a commutative operation.
# - test: The TI data object we are interested on comparing
# - reference: The TI data object we are using as reference for the comparison
tiq.helper.overlapCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.overlapCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(intersect(test$entity, reference$entity)))
}
