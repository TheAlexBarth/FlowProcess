#' #' Zip a flowprocess file
#' #'
#' #' @param folder_name The name to create zip
#' #' @param fodler_location the folder location
#' #'
#' #' @importFrom utils zip
#' #'
#' #' @exprot
#' etx_zip <- function(folder_name,
#'                     folder_location) {
#'
#'   files <- dir(folder_location, full.names = T)
#'   zip
#'   zip(paste0(folder_name,'.zip'), files)
#' }
