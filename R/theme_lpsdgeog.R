#' @title `ggplot2` theme for blog posts
#'
#' @description A theme for `ggplot2` charts for my personal blog
#'
#' @export
#'
#' @param subtitle Whether the plot has a subtitle

theme_lpsdgeog <- function(subtitle = FALSE) {
  t <- ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "Hack",
        colour = "grey40",
        margin = ggplot2::margin(6, 6, 6, 6, "pt")
      ),
      axis.title.x = ggplot2::element_text(
        hjust = 1,
        vjust = 0,
        size = 10,
        face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        hjust = 1,
        vjust = 1,
        size = 10,
        face = "bold",
        angle = 90
      ),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_line(colour = "grey90"),
      plot.margin = ggplot2::margin(6, 6, 6, 6, "pt")
    )

  if (subtitle) {
    t <- t +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          hjust = 1,
          size = 12,
          margin = ggplot2::margin(6, 6, 3, 6, "pt"),
          vjust = 0,
          lineheight = 1.1
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 1,
          size = 8,
          margin = ggplot2::margin(3, 6, 12, 6, "pt"),
          vjust = 1
        )
      )
  } else {
    t <- t +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          hjust = 1,
          size = 12,
          margin = ggplot2::margin(6, 6, 12, 6, "pt"),
          vjust = 1,
          lineheight = 1.1
        )
      )
  }

  return(t)
}
