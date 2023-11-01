####
# File processing to create importable files for ecotaxa ####
####

# Requires utilities for some features

# |- Exported Files --------------------

#' Establish Folder structure for FlowProcess
#'
#' @importFrom utils write.table
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

  project_name <- unique(etx_tsv$sample_project)[-1] #drop index for [t]

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
    sep = '\t',
    quote = FALSE
  )

  if(clean_files) {
    file.remove(paste0(root_dir, '/', meta_data$data))
    file.remove(paste0(root_dir, '/', meta_data$summary_name))
  }

}


# |- Non-exported files ----------------

#' Automatically detect the needed meta data in your root_dir
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

  # core reconstruction of tsv file
  new_data_frame <- data.frame(
    sample_project = raw_data_file$Name,
    sample_id = raw_data_file$Name,
    object_id = gsub("\\..*$", "", img_file_names),
    object_esd = raw_data_file$Diameter..ESD.,
    object_abd = raw_data_file$Diameter..ABD.,
    object_feret = raw_data_file$Diameter..FD.,
    object_blue = raw_data_file$Average.Blue,
    object_green = raw_data_file$Average.Green,
    object_red = raw_data_file$Average.Red,
    object_x = raw_data_file$Capture.X,
    object_y = raw_data_file$Capture.Y,
    object_circle_fit = raw_data_file$Circle.Fit,
    object_circularity = raw_data_file$Circularity,
    object_circularity_hu = raw_data_file$Circularity..Hu.,
    object_compactness = raw_data_file$Compactness,
    object_convex_perim = raw_data_file$Convex.Perimeter,
    object_convexivity = raw_data_file$Convexity,
    object_edge_gradient = raw_data_file$Edge.Gradient,
    object_elongation = raw_data_file$Elongation,
    object_feret_max = raw_data_file$Feret.Angle.Max,
    object_feret_min = raw_data_file$Feret.Angle.Min,
    object_fiber_curl = raw_data_file$Fiber.Curl,
    object_fiber_straight = raw_data_file$Fiber.Straightness,
    object_geodisc_length = raw_data_file$Geodesic.Length,
    object_geodisc_thickness = raw_data_file$Geodesic.Thickness,
    object_perim. = raw_data_file$Perimeter,
    object_length = raw_data_file$Length,
    object_roughness = raw_data_file$Roughness,
    object_symmetry = raw_data_file$Symmetry,
    object_width = raw_data_file$Width,
    object_Intensity = raw_data_file$Intensity,
    acq_id = raw_data_file$Name,
    acq_instrument = paste0('FlowCam SN:', raw_summary_file$SerialNo),
    acq_software = raw_summary_file$Software,
    acq_calibration_img = raw_data_file$Calibration.Image,
    acq_source_img = raw_data_file$Source.Image,
    acq_flowrate = raw_summary_file$`Flow Rate`,
    acq_mode = raw_summary_file$Mode,
    acq_vol_processed = raw_summary_file$`Sample Volume Processed`,
    acq_vol_img = raw_summary_file$`Fluid Volume Imaged`,
    process_id = raw_data_file$Name,
    process_pixel = raw_data_file$Calibration.Factor,
    process_date = raw_data_file$Date,
    process_uuid = raw_data_file$UUID,
    img_file_name = img_file_names
  )

  # head names
  header_names <- names(new_data_frame)
  col_types <- sapply(new_data_frame, assign_tsv_type)

  new_data_frame <- rbind(col_types, new_data_frame)


  return(new_data_frame)
}


#' Read the raw data file
#'
#' This is an internal function to read in
#'
#' @param root_dir the root directory
#' @param data_file_name the data file name
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


