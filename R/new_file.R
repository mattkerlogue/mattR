#' Create new files
#'
#' A convience wrapper around `fs::file_create()` and `fs::file_copy()` to
#' create new files, and (defaultly) open them in an editor.
#'
#' This function is a replacement for `rstudioapi::documentNew()`, which only
#' allows you to create files of the type `r`, `rmarkdown` and `sql`. Instead,
#' `new_file()` allows you to create any empty file of any type, or use an
#' existing file as a template, and assuming in an interactive session to open
#' that file in an editor.
#'
#' @param path Path of the file to create.
#' @param open If `TRUE` (the default) open the file.
#' @param template A template file to copy.
#' @param create_dir If `TRUE` (the default) will create missing directories.
#' @param overwrite When `NULL` (the default) will warn and prompt the user if
#'   attempting to overwrite, set to `TRUE` to force overwrite or `FALSE` to
#'   not overwrite.
#'
#' @return The path to the new file (invisibly).
#'
#' @export
new_file <- function(path, open = TRUE, template = NULL, create_dir = TRUE,
                     overwrite = NULL) {

  template_ok <- FALSE
  if (is.null(template)) {
    template_ok <- NULL
  } else {
    if (rlang::is_scalar_character(template)) {
      if (fs::file_exists(template)) {
        template_ok <- TRUE
      }
    } else {
      cli::cli_abort(c(
        "x" = "{.arg template} {.file {template}} does not exist"
      ))
    }
  }

  dir_exists <- FALSE
  if (dir.exists(dirname(path))) {
    dir_exists <- TRUE
  }

  if (!dir_exists & create_dir) {
    fs::dir_create(dirname(path))
  } else if (!dir_exists & !create_dir) {
    cli::cli_abort(c(
      "x" = "Target folder or directory does not exist",
      "i" = "Attempted to create {.file {dirname(path)}}"
    ))
  }

  if(!dir.exists(dirname(path))) {
    if (create_dir) {
      fs::dir_create(dirname(path))
    } else {
      cli::cli_abort(c(
        "x" = "Folder or directory does not exist"
      ))
    }
  }

  file_exists <- FALSE
  if (file.exists(path)) {
    file_exists <- TRUE
  }

  if (is.null(overwrite) & file_exists) {
    usr_overwrite <- tolower(readline(
      paste0("File ", path, " already exists, do you want to overwite? [Y/n]: ")
    ))
    if (usr_overwrite == "y" | usr_overwrite == "yes") {
      overwrite <- TRUE
    } else {
      overwrite <- FALSE
    }
  } else {
    overwrite <- FALSE
  }

  if (is.null(template_ok)) {
    if (overwrite & file_exists) {
      fs::file_delete(path)
    }
    fs::file_create(path)
  } else if (template_ok) {
    fs::file_copy(template, path, overwrite = overwrite)
  } else {
    cli::cli_abort(c("x" = "There is something wrong with the template"))
  }

  if (interactive() && open) {
    if (rstudioapi::isAvailable()) {
      rstudioapi::documentOpen(path)
    } else {
      open(path)
    }
  }

  invisible(path)

}
