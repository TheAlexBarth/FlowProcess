###
# Exported Functions ########
###

# |- Main Function Call ----------------

#' Duplicate Delete
#'
#' This function calls two_step_flag to get index of points
#' to delete but the main actions here are to
#'
#' @importFrom utils menu read.table
#' @importFrom grDevices dev.off png
#'
#' @param root_dir the root director of your project
#' @param write_plots function to write plots
#' @param ... arguments to pass on to flag_duplicates
#'
#' @author Alex Barth
#' @export
duplicate_deleter <- function(root_dir,
                              write_plots = TRUE,
                              ...) {

  # load in and setup the data
  metadata <- find_read_tsv(root_dir)
  project_name <- unique(metadata$sample_project)

  # check if projects are unique
  if(length(project_name) > 1) {
    stop('There are multiple projects in this one tsv - should not do that.')
  }

  # create a safety-stop to see if this process was already ran
  if(!is.null(metadata[['process_duprem_date']])) {
    go_ahead <- menu(
      c("Run it again",
        "Whoops - exit"),
      title = paste0('This Process was ran for: ',
                     project_name, ' on: ',
                     unique(metadata[['process_duprem_date']]))
    )
    if(go_ahead == 2) {
      stop('USER EXITED PROCESS')
    }
  }


  flagged_points <- tryCatch(
    expr = {
      flag_duplicates(metadata)
    },
    error = function(e) {
      warning("No flagged duplicates: ", e$message)
      return("exit")
    }
  )

  if(flagged_points[1] == 'exit') {
    return('Process Abandoned')
  }

  #write a plot to the results folder
  # This assumes you set up a file structure to emulate
  if(write_plots) {

    #check if results directory exists
    if(!dir.exists(paste0(root_dir, '/../../_results'))) {
      dir.create(paste0(root_dir, '/../../_results'))
    }

    plot_file_name <- paste0(root_dir,'/../../_results/',
                             project_name, '_duplicates.png')

    png(plot_file_name, width = 480, height = 960)
    plot_obs(metadata, flagged_points)
    dev.off()
  }

  # Delete and remove images
  remove_file_names <- metadata$img_file_name[flagged_points]
  file.remove(paste0(root_dir, '/', remove_file_names))

  # Remove from metadata
  new_metadata <- metadata[-flagged_points,]

  #assign new column to fingerprint process
  new_metadata$process_duprem_date <- as.character(Sys.Date())

  header_names <- names(new_metadata)
  col_types <- sapply(new_metadata, assign_tsv_type)

  new_metadata <- rbind(col_types, new_metadata)

  tsv_name <- paste0(root_dir, '/', 'ecotaxa_', project_name, '.tsv')

  # create
  write.table(
    new_metadata,
    file = tsv_name,
    row.names = FALSE,
    sep = '\t',
    quote = FALSE
  )
}

# |- Main Flagging system -----------

#' Pipeline for two-step flagging
#'
#' This will feed the meta through first the space detector
#' then through the feature clustering cut stage
#'
#' @param metadata the metadata frame
#' @param grid_size the resolution of grid-points to use
#' @param threshold_multiplier number of sd away from mean density
#' @param minPts The minimum number of points
#' @param features  the features to cluster on
#'
#' @author Alex Barth
#' @export
flag_duplicates <- function(metadata,
                            grid_size = 100,
                            threshold_multiplier = 2,
                            minPts = 5,
                            features = c('object_esd',
                                         'object_abd',
                                         'object_feret',
                                         'object_blue',
                                         'object_red',
                                         'object_green',
                                         'object_circle_fit',
                                         'object_circularity',
                                         'object_circularity_hu',
                                         'object_compactness',
                                         'object_convex_perim',
                                         'object_convexivity',
                                         'object_edge_gradient',
                                         'object_elongation',
                                         'object_feret_max',
                                         'object_feret_min',
                                         'object_fiber_curl',
                                         'object_fiber_straight',
                                         'object_length',
                                         'object_width',
                                         'acq_source_img')) {

  high_denisty_warning <- density_space_thresholder(metadata,
                                                    grid_size = grid_size,
                                                    threshold_multiplier = threshold_multiplier)

  space_flagged_data <- metadata[high_denisty_warning,]


  feature_index <- feature_cluster(space_flagged_data,
                                 minPts = minPts,
                                 features = features)


  final_index <- high_denisty_warning[feature_index]


  return(final_index)

}



##
# Non-exported #########
##

# |- Internal Plot option ----------

#' Plot the distribution of observations
#'
#' @importFrom graphics legend
#'
#' @param metadata the tsv file
#' @param dup_index duplicated points
#'
#' @author Alex Barth
#' @export
plot_obs <- function(metadata,
                     dup_index = NULL) {
  # Determine point colors
  point_colors <- rep("black", nrow(metadata))
  if (!is.null(dup_index)) {
    point_colors[dup_index] <- "red"
  }

  plot(metadata$object_x, metadata$object_y, col=point_colors, pch=20, cex = 0.5,
       xlab="X Coordinate", ylab="Y Coordinate", main="",
       frame.plot=FALSE, xaxs="i", yaxs="i")

  # If duplicate points exist, add a legend
  if (!is.null(dup_index)) {
    legend("top", legend=c("Duplicate", "Non-duplicate"), fill=c("red", "black"))
  }

}



# |- Thresholders ------------

#' feature clustering with HDBSCAN
#'
#' @importFrom dbscan hdbscan
#'
#' @param data the data-preferably pre filtered on proximity
#' @param minPts The minimum number of points
#' @param features  the features to cluster on
feature_cluster <- function(data,
                            minPts = 5,
                            features = c('object_esd',
                                         'object_abd',
                                         'object_feret',
                                         'object_blue',
                                         'object_red',
                                         'object_green',
                                         'object_circle_fit',
                                         'object_circularity',
                                         'object_circularity_hu',
                                         'object_compactness',
                                         'object_convex_perim',
                                         'object_convexivity',
                                         'object_edge_gradient',
                                         'object_elongation',
                                         'object_feret_max',
                                         'object_feret_min',
                                         'object_fiber_curl',
                                         'object_fiber_straight',
                                         'object_length',
                                         'object_width',
                                         'acq_source_img')) {

  # scale features
  data_features <- data[,features] |>
    scale()


  cluster_results <- hdbscan(data_features, minPts = minPts)

  first_case <- which(!duplicated(cluster_results$cluster))

  drop_clusters <- which(cluster_results$cluster != 0)

  final_cut <- drop_clusters[which(!(drop_clusters %in% first_case))]

  return(final_cut)
}


#' Kernel Density Thresholding
#'
#' This is the first step to identify spatial overlap
#'
#' @importFrom MASS kde2d bandwidth.nrd
#' @importFrom stats sd
#'
#' @param metadata a metadata file from a tsv function
#' @param grid_size the resolution of grid-points to use
#' @param threshold_multiplier number of sd away from mean density
density_space_thresholder <- function(metadata,
                                      grid_size = 100,
                                      threshold_multiplier = 2) {

  # get
  kde_output <- kde2d(metadata$object_x,
                      metadata$object_y,
                      n = grid_size)

  #loop over and index rows to z values
  nearest_index <- integer(0)
  for(r in 1:nrow(metadata)) {
    nearest_index[r] <- nearest_gridpoint(point = c(metadata$object_x[r],
                                                    metadata$object_y[r]),
                                          grid_list = list(kde_output$x,
                                                      kde_output$y))
  }

  # get vals for each point in metadata
  density_vals <- kde_output$z[nearest_index]

  threshold <- mean(kde_output$z) + threshold_multiplier*sd(kde_output$z)
  high_density <- which(density_vals > threshold)

  return(high_density)
}


#' Nearest point in a grid
#'
#' Finds the nearest point in a grid
#' This is a little tricky- it expand.grid the grid list and provides index there
#'
#' @param point the point to map
#' @param grid_list the grid to map to
nearest_gridpoint <- function(point, grid_list) {

  # Create a matrix of all grid combinations
  grid_combinations <- expand.grid(grid_list)

  # Compute Euclidean distance
  distances <- sapply(1:length(grid_list),
                      function(i) (grid_combinations[[i]] - point[i])^2) |>
    rowSums() |>
    sqrt()

  # Return the indices of the nearest grid point
  return(which.min(distances))
}



#' Find and read etx tsv
#'
#' Works in a flowprocess-formatted folder
#'
#' @param root_dir root directory
find_read_tsv <- function(root_dir) {

  path <- grep('.tsv', dir(root_dir, full.names = T), value = T)

  if(length(path) > 1) {
    stop('There are multiple tsv files in your root directory!')
  }

  return(read_etx_tsv(path))

}
