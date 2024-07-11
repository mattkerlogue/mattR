#' Easy ragg-based plots
#' 
#' A [ggplot2::ggsave] equivalent using the `ragg` package to produce
#' better png images.
#' 
#' @param file The file name of the image
#' @param plot The plot to save, uses `ggplot2::last_plot()` if left as `NULL`
#' @param width The width in pixels
#' @param height The height in pixels
#' @param overwrite Whether to overwrite existing files
#' @param ... Arguments passed down to `ragg::agg_png()`
#' 
#' @export
ragg_png <- function(file, plot = NULL, width = 600, height = 600, 
  overwrite = NULL, ...) {
  
  if(is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  if(!inherits(plot, "ggplot")) {
    cli::cli_abort(c(x = "{.arg plot} must be a {.cls ggplot} object"))
  }

  if (is.null(overwrite)) {
    if (file.exists(file)) {
      cli::cli_alert_warning(
        "{.arg file} {.fil {file}} already exists"
      )
      usr_ow <- readline("Do you wish to overwrite? (y/n) ")
      usr_ow <- tolower(substr(usr_ow, 1, 1))
      if (usr_ow == "y") {
        overwrite <- TRUE
      } else {
        overwrite <- FALSE
      }
    } else {
      overwrite <- TRUE
    }
  }

  if (!overwrite) {
    return(invisible(NULL))
  }

  if(tools::file_ext(file) != "png"){
    cli::cli_alert_warning("{.arg file} does not have a png extension, will change")
    file <- paste0(tools::file_path_sans_ext(file), ".png")
  }

  ragg::agg_png(
    filename = file, width = width, height = height, ...
  )
  grid::grid.draw(plot)
  invisible(grDevices::dev.off())

  return(invisible(file))

}