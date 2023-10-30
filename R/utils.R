###
# Exported Utilites ##########
###

# |- File Reading Utils ------------

#' A short-cut way to read etx files
#'
#' Feeds to read.table
#'
#' @param path the pathway to the file
#'
#' @author Alex Barth
#'
#' @export
read_etx_tsv <- function(path) {
  read.table(path, sep = '\t', header = T, comment.char = '[')
}


###
# Non Exported Utilities for internal functions #####
###


# |- File Formatting Utilites ----------------------

#' Assign data types
#'
#' This is necessary for proper importing into ecotaxa
#' this is written to be used with sapply
#'
#' @param data the column
assign_tsv_type <- function(data) {
  if(is.numeric(data) | is.integer(data)) {
    return('[f]')
  } else {
    return('[t]')
  }
}

#' convert something to numeric if it can be
#'
#' @param value the value to attempt to convert
as_num_if_num <- function(value) {

  if(all(grepl("^\\d+(\\.\\d+)?$", value))) {
    return(as.numeric(value))
  } else {
    return(value)
  }
}

#' Drop leading and trailing spaces
#'
#' @param value character value to drop
no_leaders_no_trailers <- function(value) {

  # if the value has mL - it should still be numeric
  value <- gsub("mL", "", value)

  value <- gsub("^\\s+|\\s+$", "", value)
  return(value)
}


#' Create
