#' Close all open source files in RStudio
#'
#' This isn't necessary as is covered by an in-built function
#' within the RStudio IDE and can be called by `cmd`+`shift`+`w` on Mac.
#'
#' @return invisibly the number of windows closed
#' @export


close_all <- function() {

  count <- 0

  # get info about the source document in focus
  # returns NULL when no open source documents
  check_source <- rstudioapi::getSourceEditorContext()

  # documentClose() closes the document in focus
  # loop closing and checking source until you get a NULL
  while (!is.null(check_source)) {
    rstudioapi::documentClose()
    count <- count + 1
    check_source <- rstudioapi::getSourceEditorContext()
  }

  if (count > 0) {
    cli::cli_alert_success("Closed {count} source windows")

  } else {
    cli::cli_alert_info("There are no open source editors")

  }

  return(invisible(count))

}
