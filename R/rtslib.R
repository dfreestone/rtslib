#' ---
#' title: rtslib.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.17"
#' purpose:
#' ---

# TODO(David): Do error checks everywhere!
#' @export
ts_check <- function(project_dir) {
  # TODO(David): Implement
  return(FALSE)
}

#' @export
ts_create_project <- function(project_id,
         project_dir,
         excel_file,
         protocol_file,
         data_dir = NULL,
         datafile_pattern = NULL,
         data_engine = mpc_read_file) {

  # expand all paths just in case (sqlite3 needs it)
  project_dir <- fs::path_expand(project_dir)
  excel_file <- fs::path_expand(excel_file)
  protocol_file <- fs::path_expand(protocol_file)
  data_dir <- fs::path_expand(data_dir)

  # first check to see if there's already a project
  if (ts_check(project_dir)) {
    stop("Project already exists")
  }

  # enforce new names for xlsx file, but not protocol files
  new_excel_file <- fs::path(project_dir, "docs",
                             glue::glue("{project_id}_excel.{fs::path_ext(excel_file)}"))
  new_protocol_file <- fs::path(project_dir, "docs", fs::path_file(protocol_file))


  # if they have data, get the paths
  has_data <- !is.null(data_dir)
  has_filepattern <- !is.null(datafile_pattern)

  if (has_data && has_filepattern) {
    data_files <- fs::dir_ls(data_dir, glob = datafile_pattern)
    new_data_files <- fs::path(project_dir, "data", "raw",
                               fs::path_file(data_files))
  } else if (has_data && !has_filepattern) {
    stop("If you supply a data directory, you must supply a filename pattern.")
  } # if

  # Create folder structure
  fs::dir_create(fs::path(project_dir))
  fs::dir_create(fs::path(project_dir, 'src'))
  fs::dir_create(fs::path(project_dir, 'docs'))
  fs::dir_create(fs::path(project_dir, 'data'))
  fs::dir_create(fs::path(project_dir, 'data', 'raw'))

  # Copy setup files
  fs::file_copy(excel_file, new_excel_file)
  fs::file_copy(protocol_file, new_protocol_file)

  # Copy data files
  if (has_data && has_filepattern) {
    fs::file_copy(data_files, new_data_files)
  }

  # If we're supposed to read the data files, read them
  if (all(fs::file_exists(new_data_files))) {
    data <- data_engine(new_data_files)
  } else {data = NULL}

  # Now create and fill the database
  db_path <- fs::path(project_dir, "data",
                      glue::glue("{project_id}_database.db"))
  db_setup(db_path, new_excel_file, new_protocol_file)

  if (!is.null(data)) { db_add_events(db_path, data) }
}

#' @export
turn_on_warnings <- function(){
  options(warn = 0)
}

#' @export
turn_off_warnings <- function() {
  options(warn = -1)
}

# Need to create
# [x] c_flushes
# [x] c_parameter_ids
# [] c_trialdef
# [] c_raster
