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
#' @param template A template file to copy
#'
#' @return The path to the new file (invisibly).
#'
#' @export
new_file <- function(path, open = TRUE, template = NULL) {

  if (is.null(template)) {
    fs::file_create(path)
  } else {
    if (rlang::is_scalar_character(template)) {
      if (fs::file_exists(template)) {
        fs::file_copy(template, path)
      }
    } else {
      cli::cli_abort(
        "x" = "{.arg template} {.file {template}} does not exist"
      )
    }
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
