#' Close all open source files
#'
#' @return invisibly the number of windows closed
#' @export
close_all <- function() {

  count <- 0

  check_source <- rstudioapi::getSourceEditorContext()

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
