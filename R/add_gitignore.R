#' Add a gitignore file
#'
#' @param path optionally the path to a directory, if not set then
#'    the current directory/RStudio project is set as the destination
#'
#' @export
add_gitignore <- function(path = NULL) {

  if (is.null(path)) {
    path <- here::here()
  } else {
    if (!fs::dir_exists(path)) {
      cli::cli_abort(c(
        x = "{.arg path} does not exist or is not a directory",
        i = "{.arg path}: {.file {path}}"
      ))
    }
  }

  gi_path <- file.path(path, ".gitignore")

  write <- NULL
  if (fs::file_exists(gi_path)) {
    ask <- readline(
      prompt = paste(
        gi_path,
        "already exists, do you want to overwrite? (y/n)", ""
      )
    )

    if (tolower(substr(ask, 1, 1)) == "y") {
      write <- TRUE
    } else {
      cli::cli_abort(c(
        x = "gitignore not written",
        i = "user aborted action"
      ))
    }

  } else {
    write <- TRUE
  }

  fs::file_copy(
    system.file("r_gitignore.txt", package = "mattR"),
    file.path(path, ".gitignore"),
    overwrite = write
  )

  cli::cli_alert_success("{.file {path}} written")


}

#' Add quarto ignores to a gitignore file
#'
#' @param gitignore optionally the path to a gitignore file, if not set
#'    then the gitignore in the current directory/RStudio project is located
#'
#' @export
add_quarto_ignores <- function(gitignore = NULL) {

  if (is.null(gitignore)) {
    gitignore <- here::here(".gitignore")
  }

  if (!fs::file_exists(gitignore)) {
    cli::cli_abort(c(
      x = "{.arg gitignore} file not found",
      i = "{.arg gitignore}: {.file {gitignore}}"
    ))
  }

  if (!grepl("\\.gitignore$", gitignore)) {
    cli::cli_abort(c(
      x = "supplied {.arg gitignore} does not appear to be a valid gitinore",
      i = "{.arg gitignore}: {.file {gitignore}}"
    ))
  }

  gi_raw <- readLines(gitignore)

  gi_detect <- sum(grepl("quarto", tolower(gi_raw)))

  if (gi_detect > 0) {
    cli::cli_abort(c(
      x = "quarto already detected in gitignore"
    ))
  }

  quarto_ignore <- c(
    "\n\n# Quarto ignores",
    "/.quarto/",
    "/_*.local",
    "/_site/",
    "/_book/"
  )

  gi_out <- c(gi_raw, quarto_ignore)

  writeLines(gi_out, gitignore)

}

