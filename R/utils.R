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
