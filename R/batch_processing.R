###
# Batch Processing Functions #####
##

#' Batch Establish Flowprocess
#'
#' This function will iterate through a directory of runs
#' and structure them to be flowprocess files
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @param root_dir the root directory. Will go into _work if present
#' @param ... additional agruments to pass to establish_flowprocess
#'
#' @export
batch_flowprocess <- function(root_dir, ...) {

  # check for work folder
  if('_work' %in% dir(root_dir)) {
    root_dir <- paste0(root_dir, '/_work')
  }

  # get list of subdirectory
  sub_dirs <- dir(root_dir, full.names = T)

  # create text progress bar
  pb <- txtProgressBar(min = 0, max = length(sub_dirs), style = 3)

  # loop through and make the process
  for(i in 1:length(sub_dirs)) {

    tryCatch(
      expr = {
        establish_flowprocess(sub_dirs[i], ...)
      },
      error = function(e) {
        warning(paste0('Not processed for ', sub_dirs[i], ' for reason: '),
                e$message)
      }
    )
    setTxtProgressBar(pb, i)
  }

  # Close the process
  close(pb)
  cat('Finished Establishing')
}

#' Batch Duplicate Deleter
#'
#' This function will iterate through a directory of runs
#' and implement duplicate deleter
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @param root_dir the root directory. Will go into _work if present
#' @param write_plots boolean to write plots to _results directory
#' @param ... additional agruments to pass to flag_duplicates
#'
#' @author Alex Barth
#' @export
batch_dup_deleter <- function(root_dir,
                              write_plots = TRUE,
                              ...) {

  # check for work folder
  if('_work' %in% dir(root_dir)) {
    root_dir <- paste0(root_dir, '/_work')
  }

  # get list of subdirectory
  sub_dirs <- dir(root_dir, full.names = T)

  # create text progress bar
  pb <- txtProgressBar(min = 0, max = length(sub_dirs), style = 3)

  # loop through and make the process
  for(i in 1:length(sub_dirs)) {

    tryCatch(
      expr = {
        duplicate_deleter(sub_dirs[i], write_plots = write_plots, ...)
      },
      error = function(e) {
        warning(paste0('Not processed for ', sub_dirs[i], ' for reason: '),
                e$message)
      }
    )
    setTxtProgressBar(pb, i)
  }

  # Close the process
  close(pb)
  cat('Finished Dupdeleter')
}
