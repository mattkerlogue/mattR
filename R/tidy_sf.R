#' Coerce sf data frames into tibbles
#'
#' A [tibble::tibble()] prints more nicely in the console than a `data.frame()`.
#' `sf`` objects can often contain many rows, making printing to the console
#' cumbersome. [sf::read_sf()] reads in geospatial data as tibbles,
#' [sf::st_read()] produces a base data.frame. `sf` objects in packages or from
#' other sources might not be tibble-like, `tidy_sf()` provides a simple
#' utility for use in pipelines to easily convert an `sf` object to a tibble
#' format.
#'
#' @param x An `sf::st_sf()` object
#' @param .verbose Whether to be noisy in checks
#'
#' @export
tidy_sf <- function(x, .verbose = FALSE) {
  if (!inherits(x, "sf")) {
    cli::cli_abort(
      c("x" = "{.arg x} must be a simple features object")
    )
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      c("x" = "{.arg x} must be a data frame")
    )
  }

  if (tibble::is_tibble(x)) {
    if (.verbose) {
      cli::cli_alert_info("{.arg x} is already a tibble, no action taken")
    }
    return(invisible(NULL))
  }

  sf::st_as_sf(tibble::as_tibble(x))
}

#' Quicker counting of sf objects
#'
#' [dplyr::count()] can take a long time on an `sf` object and may well fail
#' due to collation problems. The `sf` package provides a method for `count()`
#' which adds a `.drop_geometry` argument to reduce the run time. `count_sf()`
#' is a more explicit alternative that will always drop the geometry.
#'
#' For the `sf` method to work the package has to already be part of the call
#' stack (which, to be fair, is likely if working with `sf` objects). However,
#' you explicitly set `.drop_geometry = "TRUE"`, otherwise the `sf` method for
#' `dplyr::summarise()` will kick in which seeks to collate geometries for each
#' counting group, this creates delay and can fail if there are issues in the
#' collation process. `count_sf()` is intentionally designed to drop geometry
#' and only count features.
#'
#' @param x An `sf` object
#' @param ... Arguments to pass on to `dplyr::count()`
#'
#' @export
count_sf <- function(x, ...) {
  dplyr::count(sf::st_drop_geometry(x), ...)
}
