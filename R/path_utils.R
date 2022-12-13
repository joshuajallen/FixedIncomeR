
#' Get the path for the current source file or for the project root
#'
#'
#'
#' @param get_project_root Climb backwards from the path an look for a .Rproj file
#' @param env The enviroment in which to run - defaults to the calling frame
#'
#' @return A file path as a string
#' @export
#'
#' @examples get_source_path(get_project_root = FALSE)
get_source_path <- function(get_project_root = FALSE,
                            env = parent.frame()) {

  path_from <- ""

  #
  # First check whether we are in a batch job. If so, find the
  # file path for the script containing the function call
  #

  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)

  if (length(match) > 0) {

    file_path <- sub(needle, "", cmdArgs[match])
    path_from <- "command arguments"

  } else {

    #
    # If no "--file" flag, then attempt to get the file name for
    # a script being run from within RStudio...
    #
    file_path <- sys.frames()[[1]]$ofile
    path_from <- "system frames"

    if ( is.null(file_path) ) {

      #
      # ... and if that fails, we are probably executing at a prompt
      #

      file_path <- rstudioapi::getSourceEditorContext()$path
      path_from <- "RStudio API"
    }

  }

  file_path <- normalizePath(file_path)


  if ( exists("flog.debug") ) {
    flog.debug("Found file path from %s", path_from)
    flog.debug("File path: %s", file_path)
  }

  if ( get_project_root ) {

    search_path <- file_path
    root_path <- NULL

    while ( grepl("\\\\|/", search_path) ) {

      # Remove trailing part of file_path
      search_path <- sub("([\\\\|/][^\\\\|^/]+$)", "", search_path)

      proj_files <- list.files(search_path, pattern = "*.Rproj$")

      if( length(proj_files) > 0 ) {
        root_path = search_path
        break
      }

    }

    if ( exists("flog.debug") ) {
      flog.debug("Root path: %s", root_path)
    }

    if ( is.null(root_path) ) {
      if ( exists("flog.warn") ) {
        flog.warn("Could not find a project root folder")
      } else {
        warning("Could not find a project root folder")
      }
    }

    return(root_path)

  }

  file_path

}

