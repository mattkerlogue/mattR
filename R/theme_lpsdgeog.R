#' @title {ggplot2} theme for blog posts
#'
#' @description A theme for {ggplot2} charts for my personal blog
#'
#' @export
#'

#' @import ggplot2

theme_lpsdgeog <- function(subtitle = FALSE) {
  t <- theme_void() +
    theme(
      text = element_text(
        family = "Hack",
        colour = "grey40",
        margin = margin(6, 6, 6, 6, "pt")
      ),
      axis.title.x = element_text(
        hjust = 1,
        vjust = 0,
        size = 10,
        face = "bold"
      ),
      axis.title.y = element_text(
        hjust = 1,
        vjust = 1,
        size = 10,
        face = "bold",
        angle = 90
      ),
      axis.text = element_text(size = 8),
      panel.grid = element_line(colour = "grey90"),
      plot.margin = margin(6, 6, 6, 6, "pt")
    )

  if (subtitle) {
    t <- t + theme(
      plot.title = element_text(
        face = "bold",
        hjust = 1,
        size = 12,
        margin = margin(6, 6, 3, 6, "pt"),
        vjust = 0,
        lineheight = 1.1
        ),
      plot.subtitle = element_text(
        hjust = 1,
        size = 8,
        margin = margin(3, 6, 12, 6, "pt"),
        vjust = 1
    )
    )
  } else {
    t <- t + theme(
      plot.title = element_text(
        face = "bold",
        hjust = 1,
        size = 12,
        margin = margin(6, 6, 12, 6, "pt"),
        vjust = 1,
        lineheight = 1.1
      )
    )
  }

  return(t)

}
