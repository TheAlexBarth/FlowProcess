#' Batch Flowprocess
#'
#'



#' Establish Folder structure for FlowProcess
#'
#'
#'
#' @param root_dir The root directory
#' @param meta_data the metadata file
#' @param clean_files Boolean to remove original data/summary files
#' @param image_file_type the file format, default to '.png'
#'
#' @export
establish_flowprocess <- function(root_dir,
                                  meta_data = find_meta_files(root_dir),
                                  clean_files = T,
                                  image_file_type = '.png') {


  # read in the raw files
  raw_data_file <- read_data_file(root_dir, meta_data$data)
  raw_summary_file <- read_summary_file(root_dir, meta_data$summary_name)

  etx_tsv <- construct_etx_tsv(root_dir, raw_data_file,
                               raw_summary_file, image_file_type)
  project_name <- unique(etx_tsv$sample_project)

  # check for existing file name
  tsv_name <- paste0(root_dir, '/', 'ecotaxa_', project_name, '.tsv')
  if(file.exists(tsv_name)) {
    warning('Overwriting old tsv file!')
  }

  # create
  write.table(
    etx_tsv,
    file = tsv_name,
    row.names = FALSE,
    sep = '\t'
  )

  if(clean_files) {
    file.remove(paste0(root_dir, '/', meta_data$data))
    file.remove(paste0(root_dir, '/', meta_data$summary_name))
  }

}


#' Automatically detect a meta_data in your root_dir
#'
#' @param root_dir The Root directory
find_meta_files <- function(root_dir) {

  # get metadata names
  file_names <- dir(root_dir)
  data_name <- grep('_data.csv', file_names, value = T)
  summary_name <- grep('_summary.csv', file_names, value = T)

  # check for errors in reading the metadata names
  stopifnot("The data file is missing" = length(data_name) >= 1)
  stopifnot("There are too many data files" = length(data_name) <= 1)
  stopifnot("There is no summary file" = length(summary_name) >= 1)
  stopifnot("There is more than one summary file" = length(summary_name) <=1)


  return(list(
    data = data_name,
    summary_name = summary_name
  ))
}


#' Construct ecotaxa_tsv file
#'
#' This takes in the summary data and the data file to construct
#' an ecotaxa tsv style file
#'
#' @param root_dir the root directory
#' @param raw_data_file the raw data as a data frame
#' @param raw_summary_file the raw summary as a dataframe
#' @param image_file_type the file type of exported images
construct_etx_tsv <- function(root_dir,
                              raw_data_file,
                              raw_summary_file,
                              image_file_type = '.png') {

  img_file_names <- dir(root_dir)[grepl(paste0('\\',image_file_type,'$'),
                                               dir(root_dir))]

  new_data_frame <- data.frame(
    sample_project = raw_data_file$Name,
    sample_id = raw_data_file$Name,
    object_id = gsub("\\..*$", "", img_file_names),
    img_file_name = img_file_names
  )

  return(new_data_frame)
}


#' Read the raw data file
#'
#' This is an internal function to read in
#'
#' @param root_dir
#' @param data_file_name
read_data_file <- function(root_dir, data_file_name) {

  data_file_location <- paste0(root_dir, '/', data_file_name)

  stopifnot("The data file or root dir are inaccurate" = {
    file.exists(data_file_location)
  })

  # read in the raw data
  raw_data <- read.csv(data_file_location)

  return(raw_data)
}

#' Read the summary file
#'
#' Because the summary file is whacky - we will pull it into a list
#'
#' @param root_dir The root file directory
#' @param summary_file_name The summary file name
read_summary_file <- function(root_dir,
                            summary_file_name) {

  # load in summary file
  summary_file_loc <- paste0(root_dir, '/', summary_file_name)
  raw_file <- read.csv(summary_file_loc, row.names = NULL, header = F)

  # trim to just area of interest
  start_row <- grep('=Run Summaries=', raw_file$V1) + 1
  end_row <- grep('=Run Summaries End=', raw_file$V1) -1

  raw_file <- raw_file[c(start_row:end_row),]

  # remove those table header rows
  drop_rows <- grep(':', raw_file$V1)
  raw_file <- raw_file[-drop_rows,]

  raw_file$V1 <- gsub("\\t", "", raw_file$V1)

  out_list <- as.list(raw_file$V2)
  names(out_list) <- raw_file$V1

  out_list <- out_list |>
    lapply(no_leaders_no_trailers) |>
    lapply(as_num_if_num)

  return(out_list)
}


