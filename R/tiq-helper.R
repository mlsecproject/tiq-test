##
## Helper functions for operations on TI data.tables
##

# Operations on TI data.table objects -------------------------------------

# Returns the number of entities in the 'test' TI data that are not present
# in the 'reference' TI data. This is not commutative, as the differences are
# potentially asymmetric
#
# @param test The TI data object we are interested on comparing
# @param reference: The TI data object we are using as reference for the comparison
# @return 'numeric'
# @export
tiq.helper.differenceCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.differenceCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(setdiff(test$entity, reference$entity)))
}

# Returns the number of entities in the 'test' TI data that are ALSO present
# in the 'reference' TI data. This is a commutative operation.
#
# @param test The TI data object we are interested on comparing
# @param reference The TI data object we are using as reference for the comparison
# @return 'numeric'
# @export
tiq.helper.overlapCount <- function(test, reference) {
  if (is.null(reference$entity) || is.null(test$entity)) {
    msg = sprintf("tiq.helper.overlapCount: both reference and test datasets must have the 'entity' field")
    flog.error(msg)
    stop(msg)
  }

  return(length(intersect(test$entity, reference$entity)))
}

# used to insert missing days into test values

fixup <- function(l) {

  # GET ALL THE DAYS!

  all_days <- sort(unique(unlist(sapply(names(l), function(x) {
    names(l[[x]])
  }, USE.NAMES=FALSE))))

  # basic idea is to merge all the days with the ti feed days
  # this will automagically give us NAs for the missing days
  # and return everything in the original list format)

  fixed <- lapply(names(l), function(x) {
    df <- data.frame(date=names(l[[x]]),
                     values=l[[x]], stringsAsFactors=FALSE)
    df <- merge(df,
                data.frame(date=all_days, stringsAsFactors=FALSE),
                all.y=TRUE) %>%
      dplyr::arrange(date) %>% data.frame
    vals <- df[,"values"]
    names(vals) <- df[,"date"]
    vals
  })

  names(fixed) <- names(l)
  fixed

}
