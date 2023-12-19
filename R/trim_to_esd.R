#' Trim folders based on column
#'
#' If the flowcam autoimaging keeps toomany object of certain sizes,
#' you can delete the images here
#'
#' @param root_dir the folder of a project - needs a tsv
#' @param min_esd the minimum est to trim to
#'
#' @export
trim_to_esd <- function(root_dir, min_esd) {

  # load in and setup the data
  metadata <- find_read_tsv(root_dir)
  project_name <- unique(metadata$sample_project)

  # check if projects are unique
  if(length(project_name) > 1) {
    stop('There are multiple projects in this one tsv - should not do that.')
  }

  # find images to delete
  faulty <- which(metadata$object_esd < min_esd)
  if(length(faulty) < 0) {
    print(paste0('Nothing Trimmed on ', project_name))
    return()
  }

  drop_imgs <- metadata$img_file_name[faulty]
  files_to_remove <- paste0(root_dir, '/', drop_imgs)
  file.remove(files_to_remove)

  # Remove from metadata
  new_metadata <- metadata[-faulty,]


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
