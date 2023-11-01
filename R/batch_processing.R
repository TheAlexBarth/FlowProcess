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
batch_flowprocess <- function(root_dir, ...) {

  if('_work' %in% dir(root_dir)) {
    root_dir <- paste0(root_dir, '/_work')
  }

  sub_dirs <- dir(root_dir)

  pb <- txtProgressBar(min = 0, max = length(sub_dirs), style = 3)

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

  close(pb)

}
